# Visitor-Fact Hash Plan

## Goal

Move from ad hoc host-side shortcuts to an explicit incremental architecture where:

- rules or rule families declare which facts they depend on
- facts have stable semantic hashes
- cache invalidation is driven by fact hashes, not broad reruns
- facts stay outside the interpreter
- policy can move back toward interpreted `elm-review` code over time

This keeps the current performance wins, but makes the architecture more general and less rule-specific.


## Current State

What is already working:

- per-file `FileAnalysis` caching
- `CrossModuleSummary` fact extraction outside the interpreter
- package summary cache for review app loading
- target-project runtime cache
- exact host-backed upper-bound paths for:
  - `NoUnused.Exports`
  - `NoUnused.CustomTypeConstructors`
  - `NoUnused.CustomTypeConstructorArgs`
  - `NoUnused.Parameters`
  - `NoUnused.Variables`

Current mixed `small-12` result:

| Scenario | Runner | CLI |
|---|---:|---:|
| Warm | `0.37s` | `1.05s` |
| Warm 1-file body edit | `1.65s` | `1.06s` |
| Warm 1-file comment-only | `0.86s` | `1.09s` |
| Warm import-graph change | `1.65s` | `1.14s` |

Current bottlenecks:

- `load_review_project`
- remaining `module_rule_eval`
- outer wall-clock overhead outside traced inner stages

The old project-rule wall is mostly gone on this fixture.


## Design Principle

Use explicit fact dependencies at the same level of abstraction that visitors consume data.

Do not drive invalidation from raw AST identity.
Do not rely on generic helper-function memoization as the main mechanism.

The right unit is:

- semantic fact summary
- stable fact hash
- rule-family contribution hash


## Visitor-To-Fact Contracts

The next architectural step is to make rule dependencies explicit.

Proposed shape:

```elm
type FactSet
    = ImportShape
    | ExportShape
    | ConstructorShape
    | ConstructorUsage
    | ConstructorArgumentUsage
    | FunctionUsage
    | TypeUsage
    | ValueUsage
    | LetUsage
```

```elm
type alias RuleFactContract =
    { ruleName : String
    , factSets : List FactSet
    }
```

Initial mapping:

| Rule / family | Fact sets |
|---|---|
| `NoUnused.Exports` | `ExportShape`, `ValueUsage`, `ImportShape` |
| `NoUnused.CustomTypeConstructors` | `ConstructorShape`, `ConstructorUsage`, `ImportShape` |
| `NoUnused.CustomTypeConstructorArgs` | `ConstructorShape`, `ConstructorArgumentUsage`, `ConstructorUsage` |
| `NoUnused.Parameters` | `FunctionUsage`, `LetUsage`, `ValueUsage` |
| `NoUnused.Variables` | `ImportShape`, `TypeUsage`, `ValueUsage`, `LetUsage` |
| `NoMissingTypeAnnotation` | likely `ValueUsage`, declaration shape, signature presence |
| `NoMissingTypeAnnotationInLetIn` | likely `LetUsage`, nested function shape, signature presence |

This does not need to be per upstream visitor function. It needs to be per semantic dependency surface.


## Fact Families

We already have most of the raw material in `CrossModuleSummary`.

The next step is to split it into explicit hashed fact families.

Proposed families:

```elm
type alias ImportShapeFacts =
    { importSummaries : List ImportSummary
    , importedModules : Set String
    , importedOpenTypesByModule : Dict String (Set String)
    , moduleAliases : Dict String String
    }
```

```elm
type alias ExportShapeFacts =
    { moduleName : String
    , moduleNameRange : Range
    , isExposingAll : Bool
    , exposedValues : Dict String Range
    , exposedConstructors : Dict String (Dict String Range)
    }
```

```elm
type alias ConstructorFacts =
    { declaredConstructors : Dict String (Dict String Range)
    , constructorArgumentRanges : Dict String (Dict String (List Range))
    , expressionConstructorRefs : List QualifiedRef
    , patternConstructorRefs : List QualifiedRef
    , constructorPatternUsages : List ConstructorPatternUsage
    , comparisonConstructorRefs : List QualifiedRef
    }
```

```elm
type alias FunctionUsageFacts =
    { topLevelFunctionSummaries : List TopLevelFunctionSummary
    , nestedFunctionSummaries : List TopLevelFunctionSummary
    , letBindingSummaries : List LetBindingSummary
    , dependencyRefs : List DependencyRef
    }
```

```elm
type alias TypeUsageFacts =
    { localTypeDeclarations : Dict String LocalTypeSummary
    , typeRefs : List QualifiedRef
    }
```

These should become first-class outputs of analysis instead of staying implicit fields in one large summary blob.


## Stable Fact Hashes

The hash layer should sit on top of fact families, not on raw AST nodes.

### Why not raw AST Merkle hashing?

Because raw AST identity is too syntax-sensitive:

- comments and formatting should often not matter
- local AST churn can be larger than the semantic effect
- visitors often care about summarized relationships, not exact raw structure

### Good fit

Hash these instead:

```elm
type alias FactHashes =
    { importShapeHash : String
    , exportShapeHash : String
    , constructorFactsHash : String
    , functionUsageHash : String
    , typeUsageHash : String
    }
```

Each hash should be:

- deterministic
- order-stable
- based on canonical encoded summaries
- insensitive to irrelevant syntax

Canonicalization rules:

- sort dict keys before hashing
- sort lists where order is semantically irrelevant
- preserve order where visitor semantics depend on order
- exclude source text and AST JSON unless the fact truly depends on them

### Hash examples

`ImportShapeFacts`:

```elm
importShapeHash =
    canonicalEncodeImportShape facts
        |> FNV1a.hash
        |> String.fromInt
```

`FunctionUsageFacts`:

```elm
functionUsageHash =
    canonicalEncodeFunctionUsage facts
        |> FNV1a.hash
        |> String.fromInt
```

Later, family contribution hashes can be built from these fact hashes:

```elm
type alias RuleFamilyContributionKey =
    { family : String
    , moduleName : String
    , factHashes : List String
    }
```


## Cache Architecture

### Layer 1: File facts

Per file, cache:

- `ParsedAst`
- `ModuleSummary`
- `BodySummary`
- explicit fact families
- explicit fact hashes

Proposed shape:

```elm
type alias FileFacts =
    { sourceHash : String
    , parsedAst : ParsedAst
    , moduleSummary : ModuleSummary
    , bodySummary : BodySummary
    , importShape : ImportShapeFacts
    , exportShape : ExportShapeFacts
    , constructorFacts : ConstructorFacts
    , functionUsage : FunctionUsageFacts
    , typeUsage : TypeUsageFacts
    , factHashes : FactHashes
    }
```

### Layer 2: Rule-family contributions

Persist per-file rule-family contributions keyed by fact hashes:

```elm
type alias ContributionCacheKey =
    { family : String
    , moduleName : String
    , factHashInputs : List String
    }
```

This is where Salsa-style reuse should pay off next.

### Layer 3: Folded family results

Persist family fold outputs keyed by contribution hashes:

```elm
type alias FoldCacheKey =
    { family : String
    , contributionHashes : List String
    }
```

That allows downstream folds to stay green when recomputed contributions are unchanged.


## Backdating

Backdating should be applied at the fact family level.

If a file changes but `importShapeHash` is unchanged:

- keep the previous effective revision for `ImportShapeFacts`
- downstream `ImportersOf` consumers stay green

Likewise for:

- `constructorFactsHash`
- `functionUsageHash`
- `typeUsageHash`

This is closer to Salsa’s red-green model than our current broad cache hit/miss buckets.


## Implementation Phases

### Phase 1: Make contracts explicit

- Introduce `FactSet`
- Introduce `RuleFactContract`
- Hardcode contracts for the current hot families
- Add pure tests that every selected rule maps to the expected fact sets

Deliverable:
- no perf change required
- architecture made explicit

### Phase 2: Split `CrossModuleSummary` into hashed fact families

- Extract explicit fact-family builders
- Compute stable fact hashes
- Keep `CrossModuleSummary` temporarily as a compatibility aggregate

Deliverable:
- no behavior change
- stable hashes available for tests and future caches

### Phase 3: Add contribution cache keys from fact hashes

- Define per-family contribution key builders
- Persist/reuse contribution artifacts keyed by fact hashes
- Start with `NoUnused` families already proven hot

Deliverable:
- first direct use of fact hashes in invalidation

### Phase 4: Backdating

- If recomputed fact hashes are equal, preserve prior effective revision
- If recomputed contribution hashes are equal, preserve fold revision

Deliverable:
- smaller downstream invalidation surface

### Phase 5: Push policy back toward interpreted execution where viable

- keep facts host-side
- feed interpreted code from compact fact-backed inputs where feasible
- only retain host policy shortcuts where the interpreter path remains prohibitively expensive


## Test Strategy

This work should be test-driven before broad rollout.

### Pure tests

Add to `ReviewRunnerTest`:

1. Fact extraction tests
- imports
- exposing
- constructors
- constructor args
- function summaries
- let-binding summaries
- type refs

2. Fact hash stability tests
- comment-only changes should not change semantic fact hashes where irrelevant
- import changes should change only `importShapeHash`
- body changes should change `functionUsageHash` but not `importShapeHash`
- constructor-only changes should change `constructorFactsHash`

3. Contract tests
- each configured rule/family maps to the expected `FactSet` list

### Differential tests

Use the existing CLI diff harness pattern:

- isolated family diff
- mixed `small-12` diff
- cold / warm / body edit / comment-only / import change

### BackendTask tests

Use `Test.BackendTask` where cache behavior needs real file IO:

- persisted fact hashes reload correctly
- contribution cache keys survive process boundaries
- backdating preserves effective revision on no-op semantic edits

### Regression invariants

These should become explicit tests:

```elm
commentOnlyEdit
    |> recomputeFacts
    |> .factHashes.importShapeHash
    |> Expect.equal previous.importShapeHash
```

```elm
bodyEditWithoutImportChange
    |> recomputeFacts
    |> .factHashes.importShapeHash
    |> Expect.equal previous.importShapeHash
```

```elm
importGraphEdit
    |> recomputeFacts
    |> .factHashes.importShapeHash
    |> Expect.notEqual previous.importShapeHash
```


## Benchmark Gates

Keep the current benchmark suite as the acceptance gate:

- isolated family harnesses
- mixed `small-12`
- CLI comparison

Success criteria for this plan:

1. No correctness regressions in diff harnesses.
2. Fact hashes remain stable on comment-only and unrelated edits.
3. Fact-hash-driven invalidation reduces rerun scope measurably.
4. Mixed warm body edit and warm import change move closer to or below CLI.


## Immediate Next Step

Do not start by changing cache behavior.

Start with:

1. `FactSet`
2. `RuleFactContract`
3. explicit fact-family extraction from `CrossModuleSummary`
4. stable fact hash tests

That gives a safe foundation before contribution-cache rewrites.
