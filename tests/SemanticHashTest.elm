module SemanticHashTest exposing (suite)

import Elm.Parser
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.Node
import Expect
import SemanticHash
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "SemanticHash"
        [ importAliasTracking
        , expressionDependencyCoverage
        , depsForExpressionHonoursAliases
        ]


{-| `MutationTestRunner` uses `depsForExpression` to compute per-test-child
dependency sets for runner-index precision. If that computation ignores
import aliases, children that use `import Foo.Bar as Bar` + `Bar.baz`
end up with empty dep sets — and every mutation on `Foo.Bar` gets
silently classified as no-coverage. This surfaced as a 282-mutation
drop in FeedTests kills the moment #8 (per-child precision for inline
children) turned the previously-dormant bug on.
-}
depsForExpressionHonoursAliases : Test
depsForExpressionHonoursAliases =
    describe "depsForExpression resolves import aliases"
        [ test "`Bar.baz` via `import Foo.Bar as Bar` resolves to Foo.Bar.baz" <|
            \_ ->
                let
                    fooBarSource : String
                    fooBarSource =
                        String.join "\n"
                            [ "module Foo.Bar exposing (baz)"
                            , ""
                            , ""
                            , "baz : Int"
                            , "baz = 1"
                            , ""
                            ]

                    testSource : String
                    testSource =
                        String.join "\n"
                            [ "module MyTests exposing (probe)"
                            , ""
                            , "import Foo.Bar as Bar"
                            , ""
                            , ""
                            , "probe : Int"
                            , "probe = Bar.baz"
                            , ""
                            ]

                    index =
                        SemanticHash.buildRawIndex
                            [ { moduleName = "Foo.Bar", source = fooBarSource }
                            , { moduleName = "MyTests", source = testSource }
                            ]
                            |> SemanticHash.resolveRawIndex

                    resolver : SemanticHash.ImportResolver
                    resolver =
                        case Elm.Parser.parseToFile testSource of
                            Ok file ->
                                SemanticHash.buildImportResolver file.imports

                            Err _ ->
                                SemanticHash.emptyImportResolver

                    -- Parse the probe expression (Bar.baz) out of the test module.
                    probeExpr : Maybe (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
                    probeExpr =
                        case Elm.Parser.parseToFile testSource of
                            Ok file ->
                                file.declarations
                                    |> List.filterMap
                                        (\(Elm.Syntax.Node.Node _ decl) ->
                                            case decl of
                                                Elm.Syntax.Declaration.FunctionDeclaration func ->
                                                    Just (Elm.Syntax.Node.value func.declaration |> .expression)

                                                _ ->
                                                    Nothing
                                        )
                                    |> List.head

                            Err _ ->
                                Nothing
                in
                case probeExpr of
                    Just expr ->
                        SemanticHash.depsForExpression index resolver "MyTests" expr
                            |> Expect.equal (Set.singleton "Foo.Bar.baz")

                    Nothing ->
                        Expect.fail "failed to parse test source"
        ]


{-| Audit of expression types `extractDependencies` must track so that
Merkle propagation doesn't miss real call-edges. Each case is written
as a mutation that would be a silent false positive (fast-path
EQUIVALENT) if the dependency edge is missing.
-}
expressionDependencyCoverage : Test
expressionDependencyCoverage =
    describe "extractDependencies covers every way one decl can reference another"
        [ test "RecordUpdateExpression tracks the record-being-updated as a dependency" <|
            \_ ->
                let
                    fooSource : String
                    fooSource =
                        String.join "\n"
                            [ "module Foo exposing (baseConfig, myConfig)"
                            , ""
                            , ""
                            , "baseConfig : { a : Int, b : Int }"
                            , "baseConfig = { a = 1, b = 2 }"
                            , ""
                            , ""
                            , "myConfig : { a : Int, b : Int }"
                            , "myConfig = { baseConfig | a = 10 }"
                            , ""
                            ]

                    fooMutated : String
                    fooMutated =
                        String.join "\n"
                            [ "module Foo exposing (baseConfig, myConfig)"
                            , ""
                            , ""
                            , "baseConfig : { a : Int, b : Int }"
                            , "baseConfig = { a = 1, b = 3 }"
                            , ""
                            , ""
                            , "myConfig : { a : Int, b : Int }"
                            , "myConfig = { baseConfig | a = 10 }"
                            , ""
                            ]

                    baselineRaw =
                        SemanticHash.buildRawIndex
                            [ { moduleName = "Foo", source = fooSource } ]

                    baselineSemantic =
                        SemanticHash.resolveRawIndex baselineRaw

                    mutatedRaw =
                        SemanticHash.replaceModuleInRawIndex baselineRaw
                            { moduleName = "Foo", source = fooMutated }

                    mutatedSemantic =
                        SemanticHash.resolveRawIndexIncremental baselineSemantic mutatedRaw

                    diff =
                        SemanticHash.diffIndices baselineSemantic mutatedSemantic
                in
                diff.changed
                    |> Expect.equal (Set.fromList [ "Foo.baseConfig", "Foo.myConfig" ])
        , test "let-function name is part of the hash (rename should not collide)" <|
            \_ ->
                let
                    -- Version A: `helper` is bound in the let.
                    -- Version B: same body, but the let-binding is renamed to `other`.
                    -- The outer body still references `helper` in both versions, so
                    -- Version B is effectively broken code — its hash must differ
                    -- from Version A or a rename mutation would look equivalent.
                    aSource : String
                    aSource =
                        String.join "\n"
                            [ "module Foo exposing (foo)"
                            , ""
                            , ""
                            , "foo : Int"
                            , "foo ="
                            , "    let"
                            , "        helper x = x + 1"
                            , "    in"
                            , "    helper 5"
                            , ""
                            ]

                    bSource : String
                    bSource =
                        String.join "\n"
                            [ "module Foo exposing (foo)"
                            , ""
                            , ""
                            , "foo : Int"
                            , "foo ="
                            , "    let"
                            , "        other x = x + 1"
                            , "    in"
                            , "    helper 5"
                            , ""
                            ]

                    aRaw =
                        SemanticHash.buildRawIndex
                            [ { moduleName = "Foo", source = aSource } ]

                    bRaw =
                        SemanticHash.buildRawIndex
                            [ { moduleName = "Foo", source = bSource } ]

                    aSemantic =
                        SemanticHash.resolveRawIndex aRaw

                    bSemantic =
                        SemanticHash.resolveRawIndex bRaw

                    diff =
                        SemanticHash.diffIndices aSemantic bSemantic
                in
                diff.changed
                    |> Expect.equal (Set.fromList [ "Foo.foo" ])
        , test "let-function argument patterns are part of the hash (reorder should not collide)" <|
            \_ ->
                let
                    -- Same body (`x - y`), but argument order swapped. Semantically
                    -- different (subtraction isn't commutative), so hash must differ.
                    aSource : String
                    aSource =
                        String.join "\n"
                            [ "module Foo exposing (foo)"
                            , ""
                            , ""
                            , "foo : Int"
                            , "foo ="
                            , "    let"
                            , "        sub x y = x - y"
                            , "    in"
                            , "    sub 1 2"
                            , ""
                            ]

                    bSource : String
                    bSource =
                        String.join "\n"
                            [ "module Foo exposing (foo)"
                            , ""
                            , ""
                            , "foo : Int"
                            , "foo ="
                            , "    let"
                            , "        sub y x = x - y"
                            , "    in"
                            , "    sub 1 2"
                            , ""
                            ]

                    aRaw =
                        SemanticHash.buildRawIndex
                            [ { moduleName = "Foo", source = aSource } ]

                    bRaw =
                        SemanticHash.buildRawIndex
                            [ { moduleName = "Foo", source = bSource } ]

                    aSemantic =
                        SemanticHash.resolveRawIndex aRaw

                    bSemantic =
                        SemanticHash.resolveRawIndex bRaw

                    diff =
                        SemanticHash.diffIndices aSemantic bSemantic
                in
                diff.changed
                    |> Expect.equal (Set.fromList [ "Foo.foo" ])
        ]


importAliasTracking : Test
importAliasTracking =
    describe "dependency tracking through import aliases"
        [ test "a mutation in Foo.Bar.baz is tracked when the caller wrote `import Foo.Bar as Bar` and `Bar.baz`" <|
            \_ ->
                let
                    fooBarSource : String
                    fooBarSource =
                        String.join "\n"
                            [ "module Foo.Bar exposing (baz)"
                            , ""
                            , ""
                            , "baz : Int"
                            , "baz = 1"
                            , ""
                            ]

                    fooBarMutated : String
                    fooBarMutated =
                        String.join "\n"
                            [ "module Foo.Bar exposing (baz)"
                            , ""
                            , ""
                            , "baz : Int"
                            , "baz = 2"
                            , ""
                            ]

                    userSource : String
                    userSource =
                        String.join "\n"
                            [ "module User exposing (use)"
                            , ""
                            , "import Foo.Bar as Bar"
                            , ""
                            , ""
                            , "use : Int"
                            , "use = Bar.baz"
                            , ""
                            ]

                    baselineRaw =
                        SemanticHash.buildRawIndex
                            [ { moduleName = "Foo.Bar", source = fooBarSource }
                            , { moduleName = "User", source = userSource }
                            ]

                    baselineSemantic =
                        SemanticHash.resolveRawIndex baselineRaw

                    mutatedRaw =
                        SemanticHash.replaceModuleInRawIndex baselineRaw
                            { moduleName = "Foo.Bar", source = fooBarMutated }

                    mutatedSemantic =
                        SemanticHash.resolveRawIndexIncremental baselineSemantic mutatedRaw

                    diff =
                        SemanticHash.diffIndices baselineSemantic mutatedSemantic
                in
                diff.changed
                    |> Expect.equal (Set.fromList [ "Foo.Bar.baz", "User.use" ])
        , test "a mutation is tracked when the caller wrote `import Foo exposing (bar)` and referenced `bar` unqualified" <|
            \_ ->
                let
                    fooSource : String
                    fooSource =
                        String.join "\n"
                            [ "module Foo exposing (bar)"
                            , ""
                            , ""
                            , "bar : Int"
                            , "bar = 1"
                            , ""
                            ]

                    fooMutated : String
                    fooMutated =
                        String.join "\n"
                            [ "module Foo exposing (bar)"
                            , ""
                            , ""
                            , "bar : Int"
                            , "bar = 2"
                            , ""
                            ]

                    userSource : String
                    userSource =
                        String.join "\n"
                            [ "module User exposing (use)"
                            , ""
                            , "import Foo exposing (bar)"
                            , ""
                            , ""
                            , "use : Int"
                            , "use = bar"
                            , ""
                            ]

                    baselineRaw =
                        SemanticHash.buildRawIndex
                            [ { moduleName = "Foo", source = fooSource }
                            , { moduleName = "User", source = userSource }
                            ]

                    baselineSemantic =
                        SemanticHash.resolveRawIndex baselineRaw

                    mutatedRaw =
                        SemanticHash.replaceModuleInRawIndex baselineRaw
                            { moduleName = "Foo", source = fooMutated }

                    mutatedSemantic =
                        SemanticHash.resolveRawIndexIncremental baselineSemantic mutatedRaw

                    diff =
                        SemanticHash.diffIndices baselineSemantic mutatedSemantic
                in
                diff.changed
                    |> Expect.equal (Set.fromList [ "Foo.bar", "User.use" ])
        , test "a mutation is tracked when a later `import X.Y as X` shadows `import X` and the caller uses `X.foo`" <|
            \_ ->
                let
                    icalSource : String
                    icalSource =
                        String.join "\n"
                            [ "module Ical exposing (Status(..))"
                            , ""
                            , ""
                            , "type Status = Confirmed | Tentative"
                            , ""
                            ]

                    generatorSource : String
                    generatorSource =
                        String.join "\n"
                            [ "module Ical.Generator exposing (event)"
                            , ""
                            , ""
                            , "event : Int -> Int"
                            , "event x = x + 1"
                            , ""
                            ]

                    generatorMutated : String
                    generatorMutated =
                        String.join "\n"
                            [ "module Ical.Generator exposing (event)"
                            , ""
                            , ""
                            , "event : Int -> Int"
                            , "event x = x + 2"
                            , ""
                            ]

                    testSource : String
                    testSource =
                        String.join "\n"
                            [ "module FeedTests exposing (suite)"
                            , ""
                            , "import Ical exposing (Status(..))"
                            , "import Ical.Generator as Ical"
                            , ""
                            , ""
                            , "suite : Int"
                            , "suite = Ical.event 0"
                            , ""
                            ]

                    baselineRaw =
                        SemanticHash.buildRawIndex
                            [ { moduleName = "Ical", source = icalSource }
                            , { moduleName = "Ical.Generator", source = generatorSource }
                            , { moduleName = "FeedTests", source = testSource }
                            ]

                    baselineSemantic =
                        SemanticHash.resolveRawIndex baselineRaw

                    mutatedRaw =
                        SemanticHash.replaceModuleInRawIndex baselineRaw
                            { moduleName = "Ical.Generator", source = generatorMutated }

                    mutatedSemantic =
                        SemanticHash.resolveRawIndexIncremental baselineSemantic mutatedRaw

                    diff =
                        SemanticHash.diffIndices baselineSemantic mutatedSemantic
                in
                diff.changed
                    |> Expect.equal (Set.fromList [ "Ical.Generator.event", "FeedTests.suite" ])
        ]
