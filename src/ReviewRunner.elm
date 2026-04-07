module ReviewRunner exposing (CacheDecision(..), CacheState, CrossModuleDep(..), DeclarationCache, ReviewError, RuleDependencyProfile, RuleType(..), buildExpression, buildExpressionForRule, buildExpressionForRules, buildExpressionWithAst, buildModuleRecord, checkCache, classifyRuleSource, computeSemanticKey, conflictingPackages, decodeCacheState, encodeCacheState, encodeFileAsJson, escapeElmString, getDeclarationHashes, kernelPackages, mapErrorsToDeclarations, narrowCacheKey, parseReviewOutput, patchSource, profileForRule, reviewRunnerHelperSource, run, updateCache)

{-| Run elm-review rules via the interpreter.

Usage:
    npx elm-pages run src/ReviewRunner.elm

-}

import Ansi.Color
import Array exposing (Array)
import AstWireCodec
import BackendTask exposing (BackendTask)
import Lamdera.Wire3
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Glob as Glob
import Bytes exposing (Bytes)
import Cache
import DepGraph
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Dict exposing (Dict)
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import FastDict
import FatalError exposing (FatalError)
import Json.Decode
import FNV1a
import Json.Encode
import SemanticHash
import BackendTask.Time
import InterpreterProject exposing (InterpreterProject)
import MemoRuntime
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import Set
import Time
import Eval.Expression
import Types
import ValueCodec


type alias ReviewError =
    { ruleName : String
    , filePath : String
    , line : Int
    , column : Int
    , message : String
    }


type alias Config =
    { reviewDir : String
    , sourceDirs : List String
    , buildDirectory : Path
    , memoizedFunctions : Set.Set String
    , memoProfile : Bool
    , importersCacheMode : ImportersCacheMode
    , depsCacheMode : ImportersCacheMode
    , perfTraceJson : Maybe String
    }


type ImportersCacheMode
    = ImportersAuto
    | ImportersFresh
    | ImportersSplit


type alias InputRevision =
    { key : String
    }


type Durability
    = WorkspaceDurability
    | ReviewAppDurability
    | DependencyDurability


type alias ParsedAst =
    { astJson : String
    }


type alias ModuleSummary =
    { revision : InputRevision
    , durability : Durability
    , moduleName : String
    , imports : List String
    , aspectHashes : SemanticHash.FileAspectHashes
    }


type alias BodySummary =
    { revision : InputRevision
    , declarations : List DeclarationHashInfo
    }


type alias FileAnalysis =
    { sourceHash : String
    , parsedAst : ParsedAst
    , moduleSummary : ModuleSummary
    , bodySummary : BodySummary
    }


type alias AnalyzedTargetFile =
    { path : String
    , source : String
    , analysis : FileAnalysis
    }


type alias AnalyzedTargetFilesResult =
    { files : List AnalyzedTargetFile
    , analysisHits : Int
    , analysisMisses : Int
    , sourceBytes : Int
    , astJsonBytes : Int
    , cacheEntries : Int
    , cacheLoadBytes : Int
    , cacheStoreBytes : Int
    , readMs : Int
    , analysisMs : Int
    }


type alias ProjectCacheMetadata =
    { schemaVersion : String
    , helperHash : String
    , fileHashes : Dict String String
    }


type alias ProjectEvalOutputCache =
    { keyPath : String
    , dataPath : String
    , key : String
    }


type alias LoadedRuleCaches =
    { baseCaches : Dict String Types.Value
    , moduleEntries : Dict String (Dict String Types.Value)
    , entryCount : Int
    , bytes : Int
    }


emptyLoadedRuleCaches : LoadedRuleCaches
emptyLoadedRuleCaches =
    { baseCaches = Dict.empty
    , moduleEntries = Dict.empty
    , entryCount = 0
    , bytes = 0
    }


type alias ProjectRuleEvalResult =
    { output : String
    , memoCache : MemoRuntime.MemoCache
    , memoStats : MemoRuntime.MemoStats
    , loadedRuleCaches : LoadedRuleCaches
    , counters : Dict String Int
    }


type alias PerFileRuleKey =
    { path : String
    , narrowKey : String
    }


type alias PerFileRuleCheck =
    { path : String
    , hit : Bool
    }


type alias PerfStage =
    { name : String
    , ms : Int
    }


type alias RuleEvalPerf =
    { moduleRuleEvalMs : Int
    , projectRuleEvalMs : Int
    , counters : Dict String Int
    }


type alias ReviewRunResult =
    { output : String
    , perf : RuleEvalPerf
    }


type alias PerfTrace =
    { schemaVersion : String
    , reviewAppHash : String
    , cacheDecision : String
    , counters : Dict String Int
    , stages : List PerfStage
    }


type alias Timed a =
    { value : a
    , stage : PerfStage
    }


cacheSchemaVersion : String
cacheSchemaVersion =
    "review-runner-v6"


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.optionalKeywordArg "review-dir"
                        |> Option.map (Maybe.withDefault "review")
                        |> Option.withDescription "Review config directory (default: review)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "source-dirs"
                        |> Option.map (Maybe.map (String.split ",") >> Maybe.withDefault [ "src" ])
                        |> Option.withDescription "Source directories to review (comma-separated, default: src)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "build"
                        |> Option.map (Maybe.withDefault ".elm-review-build" >> Path.path)
                        |> Option.withDescription "Build/cache directory (default: .elm-review-build)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "memoize-functions"
                        |> Option.map (Maybe.map parseMemoizedFunctions >> Maybe.withDefault Set.empty)
                        |> Option.withDescription "Comma-separated qualified Elm function names to memoize inside the interpreter"
                    )
                |> OptionsParser.with
                    (Option.flag "memo-profile"
                        |> Option.withDescription "Write per-function memo stats to memo-profile.log in the build directory"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "importers-cache-mode"
                        |> Option.map
                            (Maybe.map parseImportersCacheMode
                                >> Maybe.withDefault ImportersAuto
                            )
                        |> Option.withDescription "ImportersOf project-rule strategy: auto, fresh, or split (default: auto)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "deps-cache-mode"
                        |> Option.map
                            (Maybe.map parseImportersCacheMode
                                >> Maybe.withDefault ImportersAuto
                            )
                        |> Option.withDescription "DependenciesOf project-rule strategy: auto, fresh, or split (default: auto)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "perf-trace-json"
                        |> Option.withDescription "Write structured review-runner perf trace JSON to the given path"
                    )
            )


type alias DeclarationHashInfo =
    { name : String
    , semanticHash : String
    , startLine : Int
    , endLine : Int
    }


parseMemoizedFunctions : String -> Set.Set String
parseMemoizedFunctions rawValue =
    rawValue
        |> String.split ","
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> Set.fromList


parseImportersCacheMode : String -> ImportersCacheMode
parseImportersCacheMode rawValue =
    case String.toLower (String.trim rawValue) of
        "auto" ->
            ImportersAuto

        "split" ->
            ImportersSplit

        _ ->
            ImportersFresh


run : Script
run =
    Script.withCliOptions programConfig (task >> BackendTask.quiet)



-- PURE HELPERS


{-| Escape a string for embedding as an Elm string literal.
Handles backslash, double quote, newline, and carriage return.
-}
escapeElmString : String -> String
escapeElmString str =
    str
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""
        |> String.replace "\n" "\\n"
        |> String.replace "\u{000D}" "\\r"


{-| Build an Elm record literal for a module: { path = "...", source = "..." }
-}
buildModuleRecord : { path : String, source : String } -> String
buildModuleRecord { path, source } =
    "{ path = \"" ++ escapeElmString path ++ "\", source = \"" ++ escapeElmString source ++ "\" }"


{-| Build the full evaluation expression.
-}
buildExpression : List { path : String, source : String } -> String
buildExpression modules =
    let
        moduleList =
            case modules of
                [] ->
                    "[]"

                _ ->
                    "[ "
                        ++ (modules
                                |> List.map buildModuleRecord
                                |> String.join ", "
                           )
                        ++ " ]"
    in
    "ReviewRunnerHelper.runReview " ++ moduleList


{-| Parse the pipe-delimited error output from ReviewRunnerHelper.
Format: RULE:name|FILE:path|LINE:n|COL:n|MSG:message
The MSG field is last and may contain pipe characters.
-}
parseReviewOutput : String -> List ReviewError
parseReviewOutput output =
    if String.isEmpty (String.trim output) then
        []

    else
        output
            |> String.lines
            |> List.filterMap parseSingleError


parseSingleError : String -> Maybe ReviewError
parseSingleError line =
    -- Split on | but MSG is last and may contain pipes
    -- Format: RULE:x|FILE:x|LINE:x|COL:x|MSG:rest...
    case String.split "|" line of
        ruleField :: fileField :: lineField :: colField :: msgParts ->
            let
                extractField prefix field =
                    if String.startsWith prefix field then
                        Just (String.dropLeft (String.length prefix) field)

                    else
                        Nothing

                -- MSG may contain | chars, so rejoin the remaining parts
                msgField =
                    String.join "|" msgParts
            in
            case
                ( extractField "RULE:" ruleField
                , extractField "FILE:" fileField
                , ( extractField "LINE:" lineField |> Maybe.andThen String.toInt
                  , extractField "COL:" colField |> Maybe.andThen String.toInt
                  , extractField "MSG:" msgField
                  )
                )
            of
                ( Just ruleName, Just filePath, ( Just lineNum, Just colNum, Just message ) ) ->
                    Just
                        { ruleName = ruleName
                        , filePath = filePath
                        , line = lineNum
                        , column = colNum
                        , message = message
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Classification of an elm-review rule as module-scoped or project-scoped.
Module rules can be evaluated per-file (independent). Project rules need
the full project for cross-module analysis.
-}
type RuleType
    = ModuleRule
    | ProjectRule


type alias RuleInfo =
    { index : Int
    , name : String
    , ruleType : RuleType
    }


type alias LoadedReviewProject =
    { project : InterpreterProject
    , counters : Dict String Int
    }


{-| Classify a rule by parsing its source code.
Looks for `newModuleRuleSchema` vs `newProjectRuleSchema` in the source.
Defaults to ProjectRule (conservative — never incorrectly caches).
-}
classifyRuleSource : String -> RuleType
classifyRuleSource source =
    if String.contains "newModuleRuleSchema" source then
        ModuleRule

    else
        -- Default to ProjectRule (conservative). This includes:
        -- - Rules using newProjectRuleSchema
        -- - Unrecognized source patterns
        ProjectRule


{-| What cross-module information a rule depends on.
-}
type CrossModuleDep
    = NoCrossModule
    | ImportersOf
    | DependenciesOf
    | FullProject


{-| Which sub-AST aspects a rule depends on.
-}
type alias RuleDependencyProfile =
    { expressionDep : Bool
    , declarationNameDep : Bool
    , importDep : Bool
    , exposingDep : Bool
    , customTypeDep : Bool
    , crossModuleDep : CrossModuleDep
    }


{-| Get the dependency profile for a known rule name.
Unknown rules get a conservative profile (all True, FullProject).
-}
profileForRule : String -> RuleDependencyProfile
profileForRule ruleName =
    case ruleName of
        "NoDebug.Log" ->
            { expressionDep = True, declarationNameDep = False, importDep = True, exposingDep = False, customTypeDep = False, crossModuleDep = NoCrossModule }

        "NoDebug.TodoOrToString" ->
            { expressionDep = True, declarationNameDep = False, importDep = True, exposingDep = False, customTypeDep = False, crossModuleDep = NoCrossModule }

        "NoExposingEverything" ->
            { expressionDep = False, declarationNameDep = True, importDep = False, exposingDep = True, customTypeDep = False, crossModuleDep = NoCrossModule }

        "NoMissingTypeAnnotation" ->
            { expressionDep = False, declarationNameDep = True, importDep = False, exposingDep = False, customTypeDep = False, crossModuleDep = NoCrossModule }

        "NoMissingTypeAnnotationInLetIn" ->
            { expressionDep = True, declarationNameDep = False, importDep = False, exposingDep = False, customTypeDep = False, crossModuleDep = NoCrossModule }

        "NoUnused.Patterns" ->
            { expressionDep = True, declarationNameDep = True, importDep = False, exposingDep = False, customTypeDep = False, crossModuleDep = NoCrossModule }

        "NoUnused.Exports" ->
            { expressionDep = True, declarationNameDep = True, importDep = True, exposingDep = True, customTypeDep = False, crossModuleDep = ImportersOf }

        "NoUnused.CustomTypeConstructors" ->
            { expressionDep = True, declarationNameDep = True, importDep = True, exposingDep = True, customTypeDep = True, crossModuleDep = ImportersOf }

        "NoUnused.CustomTypeConstructorArgs" ->
            { expressionDep = True, declarationNameDep = True, importDep = True, exposingDep = True, customTypeDep = True, crossModuleDep = ImportersOf }

        "NoUnused.Variables" ->
            { expressionDep = True, declarationNameDep = True, importDep = True, exposingDep = True, customTypeDep = True, crossModuleDep = DependenciesOf }

        "NoUnused.Parameters" ->
            { expressionDep = True, declarationNameDep = True, importDep = True, exposingDep = True, customTypeDep = False, crossModuleDep = ImportersOf }

        _ ->
            -- Conservative: depend on everything
            { expressionDep = True, declarationNameDep = True, importDep = True, exposingDep = True, customTypeDep = True, crossModuleDep = FullProject }


{-| Compute a narrow cache key for a (rule, file) pair.

Only includes the AST aspects the rule actually inspects.
For cross-module rules, includes relevant context from other files.
-}
narrowCacheKey :
    RuleDependencyProfile
    -> String
    -> SemanticHash.FileAspectHashes
    -> Dict String SemanticHash.FileAspectHashes
    -> DepGraph.Graph
    -> String
narrowCacheKey profile filePath fileHashes allFileHashes depGraph =
    let
        localParts =
            [ if profile.expressionDep then
                fileHashes.expressionsHash

              else
                ""
            , if profile.declarationNameDep then
                fileHashes.declNamesHash

              else
                ""
            , if profile.importDep then
                fileHashes.importsHash

              else
                ""
            , if profile.exposingDep then
                fileHashes.exposingHash

              else
                ""
            , if profile.customTypeDep then
                fileHashes.customTypesHash

              else
                ""
            ]
                |> List.filter (not << String.isEmpty)
                |> String.join "|"

        localKey =
            FNV1a.hash localParts |> String.fromInt

        crossModuleKey =
            case profile.crossModuleDep of
                NoCrossModule ->
                    ""

                ImportersOf ->
                    DepGraph.directReverseDeps depGraph filePath
                        |> Set.toList
                        |> List.sort
                        |> List.filterMap (\p -> Dict.get p allFileHashes)
                        |> List.map (\h -> h.importsHash ++ h.expressionsHash)
                        |> String.join "|"
                        |> FNV1a.hash
                        |> String.fromInt

                DependenciesOf ->
                    DepGraph.transitiveDeps depGraph filePath
                        |> Set.toList
                        |> List.sort
                        |> List.filterMap (\p -> Dict.get p allFileHashes)
                        |> List.map (narrowAspects profile)
                        |> String.join "|"
                        |> FNV1a.hash
                        |> String.fromInt

                FullProject ->
                    allFileHashes
                        |> Dict.toList
                        |> List.sortBy Tuple.first
                        |> List.map (\( _, h ) -> narrowAspects profile h)
                        |> String.join "|"
                        |> FNV1a.hash
                        |> String.fromInt
    in
    localKey ++ "|" ++ crossModuleKey


{-| Merge dependency profiles for a group of rules into a combined profile.
-}
mergeProfiles : CrossModuleDep -> List { a | name : String } -> RuleDependencyProfile
mergeProfiles crossModuleDep rules =
    rules
        |> List.foldl
            (\rule acc ->
                let
                    p =
                        profileForRule rule.name
                in
                { acc
                    | expressionDep = acc.expressionDep || p.expressionDep
                    , declarationNameDep = acc.declarationNameDep || p.declarationNameDep
                    , importDep = acc.importDep || p.importDep
                    , exposingDep = acc.exposingDep || p.exposingDep
                    , customTypeDep = acc.customTypeDep || p.customTypeDep
                }
            )
            { expressionDep = False, declarationNameDep = False, importDep = False, exposingDep = False, customTypeDep = False, crossModuleDep = crossModuleDep }


importersNeedsImporterContext : String -> Bool
importersNeedsImporterContext ruleName =
    case ruleName of
        "NoUnused.Exports" ->
            False

        "NoUnused.CustomTypeConstructorArgs" ->
            False

        "NoUnused.CustomTypeConstructors" ->
            True

        "NoUnused.Parameters" ->
            True

        _ ->
            True


buildPerFileRuleKeys :
    String
    -> String
    -> List { a | index : Int }
    -> RuleDependencyProfile
    -> List AnalyzedTargetFile
    -> Dict String SemanticHash.FileAspectHashes
    -> DepGraph.Graph
    -> List PerFileRuleKey
buildPerFileRuleKeys keyPrefix helperHash rules profile allFileContents allFileAspectHashes depGraph =
    allFileContents
        |> List.map
            (\file ->
                let
                    fileHashes =
                        Dict.get file.path allFileAspectHashes
                            |> Maybe.withDefault file.analysis.moduleSummary.aspectHashes
                in
                { path = file.path
                , narrowKey =
                    keyPrefix ++ "|" ++ helperHash ++ "|"
                        ++ (rules |> List.map (.index >> String.fromInt) |> String.join ",")
                        ++ "|" ++ narrowCacheKey profile file.path fileHashes allFileAspectHashes depGraph
                }
            )


{-| Extract only the aspects a rule depends on from FileAspectHashes, joined as a string.
-}
narrowAspects : RuleDependencyProfile -> SemanticHash.FileAspectHashes -> String
narrowAspects profile h =
    [ if profile.expressionDep then
        h.expressionsHash

      else
        ""
    , if profile.declarationNameDep then
        h.declNamesHash

      else
        ""
    , if profile.importDep then
        h.importsHash

      else
        ""
    , if profile.exposingDep then
        h.exposingHash

      else
        ""
    , if profile.customTypeDep then
        h.customTypesHash

      else
        ""
    ]
        |> List.filter (not << String.isEmpty)
        |> String.join "|"


{-| Compute a semantic cache key for a set of source files.

Uses SemanticHash to compute Merkle-style hashes for each declaration
in each file. Whitespace and comment changes don't affect the hash —
only meaningful AST changes do.

This runs on the elm-build side (native compilation, fast) — NOT
through the interpreter.

-}
computeSemanticKey : List { path : String, source : String } -> String
computeSemanticKey files =
    let
        perFileHashes : List String
        perFileHashes =
            files
                |> List.sortBy .path
                |> List.map
                    (\{ path, source } ->
                        let
                            -- Use FileAspectHashes.fullHash which covers ALL AST aspects:
                            -- expressions, declarations, imports, exports, custom types.
                            -- This ensures import-only and export-only changes are detected.
                            aspects =
                                SemanticHash.computeAspectHashesFromSource source
                        in
                        path ++ "|" ++ aspects.fullHash
                    )
    in
    perFileHashes
        |> String.join "\n"
        |> FNV1a.hash
        |> String.fromInt


computeSemanticKeyFromAnalyses : List AnalyzedTargetFile -> String
computeSemanticKeyFromAnalyses files =
    files
        |> List.sortBy .path
        |> List.map (\file -> file.path ++ "|" ++ file.analysis.moduleSummary.aspectHashes.fullHash)
        |> String.join "\n"
        |> FNV1a.hash
        |> String.fromInt


{-| Per-declaration cache entry: maps each top-level declaration
to its semantic hash and any review errors within its range.
-}
type alias DeclarationCache =
    Dict String { semanticHash : String, errors : List ReviewError }


hashSourceContents : String -> String
hashSourceContents source =
    FNV1a.hash source |> String.fromInt


moduleSummaryRevisionKey : String -> List String -> SemanticHash.FileAspectHashes -> String
moduleSummaryRevisionKey moduleName imports aspectHashes =
    [ moduleName
    , String.join "," imports
    , aspectHashes.fullHash
    ]
        |> String.join "|"
        |> FNV1a.hash
        |> String.fromInt


bodySummaryRevisionKey : List DeclarationHashInfo -> String
bodySummaryRevisionKey declarations =
    declarations
        |> List.map
            (\declaration ->
                declaration.name ++ ":" ++ declaration.semanticHash
            )
        |> String.join "|"
        |> FNV1a.hash
        |> String.fromInt


moduleNameFromFile : Elm.Syntax.File.File -> String
moduleNameFromFile file =
    case Node.value file.moduleDefinition of
        Elm.Syntax.Module.NormalModule { moduleName } ->
            Node.value moduleName |> String.join "."

        Elm.Syntax.Module.PortModule { moduleName } ->
            Node.value moduleName |> String.join "."

        Elm.Syntax.Module.EffectModule { moduleName } ->
            Node.value moduleName |> String.join "."


importsFromFile : Elm.Syntax.File.File -> List String
importsFromFile file =
    file.imports
        |> List.map
            (\(Node _ imp) ->
                Node.value imp.moduleName |> String.join "."
            )


getDeclarationHashesFromFile : Elm.Syntax.File.File -> List DeclarationHashInfo
getDeclarationHashesFromFile file =
    let
        index =
            SemanticHash.buildIndexFromFile file

        rangeMap : Dict String Range
        rangeMap =
            file.declarations
                |> List.filterMap
                    (\(Node range decl) ->
                        case decl of
                            FunctionDeclaration func ->
                                Just ( Node.value (Node.value func.declaration).name, range )

                            _ ->
                                Nothing
                    )
                |> Dict.fromList
    in
    index
        |> Dict.toList
        |> List.filterMap
            (\( name, info ) ->
                Dict.get name rangeMap
                    |> Maybe.map
                        (\range ->
                            { name = name
                            , semanticHash = info.semanticHash
                            , startLine = range.start.row
                            , endLine = range.end.row
                            }
                        )
            )


{-| Get per-declaration semantic hashes and line ranges for a source file.
Returns (declarationName, semanticHash, startLine, endLine) for each
top-level function declaration.
-}
getDeclarationHashes : String -> List DeclarationHashInfo
getDeclarationHashes source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            getDeclarationHashesFromFile file

        Err _ ->
            []


{-| Map review errors to their enclosing top-level declarations by line range.
Errors outside any declaration go into the "__module__" bucket.
-}
mapErrorsToDeclarations :
    List { name : String, semanticHash : String, startLine : Int, endLine : Int }
    -> List ReviewError
    -> Dict String { semanticHash : String, errors : List ReviewError }
mapErrorsToDeclarations declarations errors =
    let
        emptyCache : Dict String { semanticHash : String, errors : List ReviewError }
        emptyCache =
            declarations
                |> List.map (\d -> ( d.name, { semanticHash = d.semanticHash, errors = [] } ))
                |> Dict.fromList

        findDeclaration : Int -> Maybe String
        findDeclaration line =
            declarations
                |> List.filter (\d -> line >= d.startLine && line <= d.endLine)
                |> List.head
                |> Maybe.map .name
    in
    errors
        |> List.foldl
            (\err acc ->
                let
                    declName =
                        findDeclaration err.line
                            |> Maybe.withDefault "__module__"
                in
                Dict.update declName
                    (\existing ->
                        case existing of
                            Just entry ->
                                Just { entry | errors = entry.errors ++ [ err ] }

                            Nothing ->
                                -- Module-level error (outside any declaration)
                                Just { semanticHash = "__module__", errors = [ err ] }
                    )
                    acc
            )
            emptyCache


{-| The full cache state: per-file, per-declaration.
    filePath → (declarationName → { semanticHash, errors })
-}
type alias CacheState =
    Dict String DeclarationCache


{-| Result of checking the cache against current source files.
-}
type CacheDecision
    = FullCacheHit (List ReviewError)
    | PartialMiss
        { cachedErrors : List ReviewError
        , staleFiles : List String
        }
    | ColdMiss (List String)


{-| Check the per-declaration cache against current source files.

For each file, computes current declaration hashes and compares to cached.
If ALL declarations in ALL files match → FullCacheHit.
If some files have mismatched declarations → PartialMiss (with cached errors
for unchanged files and a list of files needing re-evaluation).
If no cache exists → ColdMiss.

-}
checkCache :
    CacheState
    -> List { path : String, source : String }
    -> CacheDecision
checkCache cache files =
    let
        fileResults =
            files
                |> List.map
                    (\{ path, source } ->
                        let
                            currentHashes =
                                getDeclarationHashes source
                                    |> List.map (\d -> ( d.name, d.semanticHash ))
                                    |> Dict.fromList

                            cachedFile =
                                Dict.get path cache
                        in
                        case cachedFile of
                            Nothing ->
                                -- No cache for this file at all
                                { path = path, status = FileMiss, cachedErrors = [] }

                            Just fileCache ->
                                let
                                    -- Exclude __module__ and __fileAspects__ from declaration comparison
                                    fileCacheDecls =
                                        fileCache
                                            |> Dict.remove "__module__"
                                            |> Dict.remove "__fileAspects__"

                                    -- Check declaration hashes match
                                    declsMatch =
                                        Dict.foldl
                                            (\name hash acc ->
                                                acc
                                                    && (Dict.get name fileCacheDecls
                                                            |> Maybe.map (\entry -> entry.semanticHash == hash)
                                                            |> Maybe.withDefault False
                                                       )
                                            )
                                            (Dict.size currentHashes == Dict.size fileCacheDecls)
                                            currentHashes

                                    -- Also check file-level aspects (imports, exports, custom types)
                                    currentFullHash =
                                        (SemanticHash.computeAspectHashesFromSource source).fullHash

                                    cachedFullHash =
                                        Dict.get "__fileAspects__" fileCache
                                            |> Maybe.map .semanticHash
                                            |> Maybe.withDefault ""

                                    allMatch =
                                        declsMatch && (currentFullHash == cachedFullHash)
                                in
                                if allMatch then
                                    { path = path
                                    , status = FileHit
                                    , cachedErrors =
                                        fileCache
                                            |> Dict.values
                                            |> List.concatMap .errors
                                    }

                                else
                                    { path = path, status = FileMiss, cachedErrors = [] }
                    )

        allHit =
            List.all (\r -> r.status == FileHit) fileResults

        staleFiles =
            fileResults |> List.filter (\r -> r.status == FileMiss) |> List.map .path

        cachedErrors =
            fileResults |> List.filter (\r -> r.status == FileHit) |> List.concatMap .cachedErrors
    in
    if List.isEmpty (Dict.keys cache) then
        ColdMiss (List.map .path files)

    else if allHit then
        FullCacheHit cachedErrors

    else
        PartialMiss { cachedErrors = cachedErrors, staleFiles = staleFiles }


type FileStatus
    = FileHit
    | FileMiss


{-| Update the cache with fresh results from a review run.
Merges new results for re-evaluated files, keeps cached entries for others.
-}
updateCache :
    CacheState
    -> List { path : String, source : String }
    -> List ReviewError
    -> CacheState
updateCache previousCache files errors =
    let
        errorsByFile =
            errors
                |> List.foldl
                    (\err acc ->
                        Dict.update err.filePath
                            (\existing -> Just (err :: Maybe.withDefault [] existing))
                            acc
                    )
                    Dict.empty
    in
    files
        |> List.foldl
            (\{ path, source } acc ->
                let
                    declarations =
                        getDeclarationHashes source

                    fileErrors =
                        Dict.get path errorsByFile |> Maybe.withDefault []

                    fileCache =
                        mapErrorsToDeclarations declarations fileErrors

                    -- Store full file aspect hash for detecting non-declaration changes
                    fullHash =
                        (SemanticHash.computeAspectHashesFromSource source).fullHash

                    fileCacheWithAspects =
                        Dict.insert "__fileAspects__"
                            { semanticHash = fullHash, errors = [] }
                            fileCache
                in
                Dict.insert path fileCacheWithAspects acc
            )
            previousCache


checkCacheWithAnalyses :
    CacheState
    -> List AnalyzedTargetFile
    -> CacheDecision
checkCacheWithAnalyses cache files =
    let
        fileResults =
            files
                |> List.map
                    (\file ->
                        let
                            currentHashes =
                                file.analysis.bodySummary.declarations
                                    |> List.map (\decl -> ( decl.name, decl.semanticHash ))
                                    |> Dict.fromList

                            cachedFile =
                                Dict.get file.path cache
                        in
                        case cachedFile of
                            Nothing ->
                                { path = file.path, status = FileMiss, cachedErrors = [] }

                            Just fileCache ->
                                let
                                    fileCacheDecls =
                                        fileCache
                                            |> Dict.remove "__module__"
                                            |> Dict.remove "__fileAspects__"

                                    declsMatch =
                                        Dict.foldl
                                            (\name hash acc ->
                                                acc
                                                    && (Dict.get name fileCacheDecls
                                                            |> Maybe.map (\entry -> entry.semanticHash == hash)
                                                            |> Maybe.withDefault False
                                                       )
                                            )
                                            (Dict.size currentHashes == Dict.size fileCacheDecls)
                                            currentHashes

                                    currentFullHash =
                                        file.analysis.moduleSummary.aspectHashes.fullHash

                                    cachedFullHash =
                                        Dict.get "__fileAspects__" fileCache
                                            |> Maybe.map .semanticHash
                                            |> Maybe.withDefault ""

                                    allMatch =
                                        declsMatch && (currentFullHash == cachedFullHash)
                                in
                                if allMatch then
                                    { path = file.path
                                    , status = FileHit
                                    , cachedErrors =
                                        fileCache
                                            |> Dict.values
                                            |> List.concatMap .errors
                                    }

                                else
                                    { path = file.path, status = FileMiss, cachedErrors = [] }
                    )

        allHit =
            List.all (\result -> result.status == FileHit) fileResults

        staleFiles =
            fileResults |> List.filter (\result -> result.status == FileMiss) |> List.map .path

        cachedErrors =
            fileResults |> List.filter (\result -> result.status == FileHit) |> List.concatMap .cachedErrors
    in
    if List.isEmpty (Dict.keys cache) then
        ColdMiss (List.map .path files)

    else if allHit then
        FullCacheHit cachedErrors

    else
        PartialMiss { cachedErrors = cachedErrors, staleFiles = staleFiles }


updateCacheWithAnalyses :
    CacheState
    -> List AnalyzedTargetFile
    -> List ReviewError
    -> CacheState
updateCacheWithAnalyses previousCache files errors =
    let
        errorsByFile =
            errors
                |> List.foldl
                    (\err acc ->
                        Dict.update err.filePath
                            (\existing -> Just (err :: Maybe.withDefault [] existing))
                            acc
                    )
                    Dict.empty
    in
    files
        |> List.foldl
            (\file acc ->
                let
                    fileErrors =
                        Dict.get file.path errorsByFile |> Maybe.withDefault []

                    fileCache =
                        mapErrorsToDeclarations file.analysis.bodySummary.declarations fileErrors

                    fileCacheWithAspects =
                        Dict.insert "__fileAspects__"
                            { semanticHash = file.analysis.moduleSummary.aspectHashes.fullHash, errors = [] }
                            fileCache
                in
                Dict.insert file.path fileCacheWithAspects acc
            )
            previousCache


{-| Encode CacheState to a JSON string for persistence to disk.
-}
encodeCacheState : CacheState -> String
encodeCacheState cache =
    cache
        |> Dict.toList
        |> Json.Encode.list
            (\( filePath, declCache ) ->
                Json.Encode.object
                    [ ( "file", Json.Encode.string filePath )
                    , ( "declarations"
                      , declCache
                            |> Dict.toList
                            |> Json.Encode.list
                                (\( declName, entry ) ->
                                    Json.Encode.object
                                        [ ( "name", Json.Encode.string declName )
                                        , ( "hash", Json.Encode.string entry.semanticHash )
                                        , ( "errors"
                                          , entry.errors
                                                |> Json.Encode.list
                                                    (\err ->
                                                        Json.Encode.object
                                                            [ ( "rule", Json.Encode.string err.ruleName )
                                                            , ( "file", Json.Encode.string err.filePath )
                                                            , ( "line", Json.Encode.int err.line )
                                                            , ( "col", Json.Encode.int err.column )
                                                            , ( "msg", Json.Encode.string err.message )
                                                            ]
                                                    )
                                          )
                                        ]
                                )
                      )
                    ]
            )
        |> Json.Encode.encode 0


{-| Decode CacheState from a JSON string (read from disk).
-}
decodeCacheState : String -> Maybe CacheState
decodeCacheState json =
    let
        errorDecoder =
            Json.Decode.map5
                (\rule file line col msg -> { ruleName = rule, filePath = file, line = line, column = col, message = msg })
                (Json.Decode.field "rule" Json.Decode.string)
                (Json.Decode.field "file" Json.Decode.string)
                (Json.Decode.field "line" Json.Decode.int)
                (Json.Decode.field "col" Json.Decode.int)
                (Json.Decode.field "msg" Json.Decode.string)

        entryDecoder =
            Json.Decode.map2 Tuple.pair
                (Json.Decode.field "name" Json.Decode.string)
                (Json.Decode.map2 (\hash errors -> { semanticHash = hash, errors = errors })
                    (Json.Decode.field "hash" Json.Decode.string)
                    (Json.Decode.field "errors" (Json.Decode.list errorDecoder))
                )

        fileDecoder =
            Json.Decode.map2 Tuple.pair
                (Json.Decode.field "file" Json.Decode.string)
                (Json.Decode.field "declarations" (Json.Decode.list entryDecoder)
                    |> Json.Decode.map Dict.fromList
                )

        decoder =
            Json.Decode.list fileDecoder
                |> Json.Decode.map Dict.fromList
    in
    Json.Decode.decodeString decoder json
        |> Result.toMaybe


encodeFileAspectHashes : SemanticHash.FileAspectHashes -> Json.Encode.Value
encodeFileAspectHashes hashes =
    Json.Encode.object
        [ ( "expressionsHash", Json.Encode.string hashes.expressionsHash )
        , ( "declNamesHash", Json.Encode.string hashes.declNamesHash )
        , ( "importsHash", Json.Encode.string hashes.importsHash )
        , ( "exposingHash", Json.Encode.string hashes.exposingHash )
        , ( "customTypesHash", Json.Encode.string hashes.customTypesHash )
        , ( "fullHash", Json.Encode.string hashes.fullHash )
        ]


fileAspectHashesDecoder : Json.Decode.Decoder SemanticHash.FileAspectHashes
fileAspectHashesDecoder =
    Json.Decode.map6
        (\expressionsHash declNamesHash importsHash exposingHash customTypesHash fullHash ->
            { expressionsHash = expressionsHash
            , declNamesHash = declNamesHash
            , importsHash = importsHash
            , exposingHash = exposingHash
            , customTypesHash = customTypesHash
            , fullHash = fullHash
            }
        )
        (Json.Decode.field "expressionsHash" Json.Decode.string)
        (Json.Decode.field "declNamesHash" Json.Decode.string)
        (Json.Decode.field "importsHash" Json.Decode.string)
        (Json.Decode.field "exposingHash" Json.Decode.string)
        (Json.Decode.field "customTypesHash" Json.Decode.string)
        (Json.Decode.field "fullHash" Json.Decode.string)


durabilityToString : Durability -> String
durabilityToString durability =
    case durability of
        WorkspaceDurability ->
            "workspace"

        ReviewAppDurability ->
            "review-app"

        DependencyDurability ->
            "dependency"


durabilityDecoder : Json.Decode.Decoder Durability
durabilityDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\value ->
                case value of
                    "workspace" ->
                        Json.Decode.succeed WorkspaceDurability

                    "review-app" ->
                        Json.Decode.succeed ReviewAppDurability

                    "dependency" ->
                        Json.Decode.succeed DependencyDurability

                    _ ->
                        Json.Decode.fail ("Unknown durability: " ++ value)
            )


encodeFileAnalysisCache : Dict String FileAnalysis -> String
encodeFileAnalysisCache cache =
    cache
        |> Dict.toList
        |> Json.Encode.list
            (\( filePath, analysis ) ->
                Json.Encode.object
                    [ ( "file", Json.Encode.string filePath )
                    , ( "sourceHash", Json.Encode.string analysis.sourceHash )
                    , ( "parsedAst"
                      , Json.Encode.object
                            [ ( "astJson", Json.Encode.string analysis.parsedAst.astJson ) ]
                      )
                    , ( "moduleSummary"
                      , Json.Encode.object
                            [ ( "revision", Json.Encode.object [ ( "key", Json.Encode.string analysis.moduleSummary.revision.key ) ] )
                            , ( "durability", Json.Encode.string (durabilityToString analysis.moduleSummary.durability) )
                            , ( "moduleName", Json.Encode.string analysis.moduleSummary.moduleName )
                            , ( "imports", Json.Encode.list Json.Encode.string analysis.moduleSummary.imports )
                            , ( "aspectHashes", encodeFileAspectHashes analysis.moduleSummary.aspectHashes )
                            ]
                      )
                    , ( "bodySummary"
                      , Json.Encode.object
                            [ ( "revision", Json.Encode.object [ ( "key", Json.Encode.string analysis.bodySummary.revision.key ) ] )
                            , ( "declarations"
                              , analysis.bodySummary.declarations
                                    |> Json.Encode.list
                                        (\decl ->
                                            Json.Encode.object
                                                [ ( "name", Json.Encode.string decl.name )
                                                , ( "semanticHash", Json.Encode.string decl.semanticHash )
                                                , ( "startLine", Json.Encode.int decl.startLine )
                                                , ( "endLine", Json.Encode.int decl.endLine )
                                                ]
                                        )
                              )
                            ]
                      )
                    ]
            )
        |> Json.Encode.encode 0


decodeFileAnalysisCache : String -> Maybe (Dict String FileAnalysis)
decodeFileAnalysisCache json =
    let
        declarationDecoder =
            Json.Decode.map4
                (\name semanticHash startLine endLine ->
                    { name = name
                    , semanticHash = semanticHash
                    , startLine = startLine
                    , endLine = endLine
                    }
                )
                (Json.Decode.field "name" Json.Decode.string)
                (Json.Decode.field "semanticHash" Json.Decode.string)
                (Json.Decode.field "startLine" Json.Decode.int)
                (Json.Decode.field "endLine" Json.Decode.int)

        analysisDecoder =
            Json.Decode.map4
                (\filePath sourceHash parsedAst summaries ->
                    ( filePath
                    , { sourceHash = sourceHash
                      , parsedAst = parsedAst
                      , moduleSummary = summaries.moduleSummary
                      , bodySummary = summaries.bodySummary
                      }
                    )
                )
                (Json.Decode.field "file" Json.Decode.string)
                (Json.Decode.field "sourceHash" Json.Decode.string)
                (Json.Decode.field "parsedAst"
                    (Json.Decode.map ParsedAst (Json.Decode.field "astJson" Json.Decode.string))
                )
                (Json.Decode.map2
                    (\moduleSummary bodySummary ->
                        { moduleSummary = moduleSummary
                        , bodySummary = bodySummary
                        }
                    )
                    (Json.Decode.field "moduleSummary"
                        (Json.Decode.map5
                            (\revision durability moduleName imports aspectHashes ->
                                { revision = revision
                                , durability = durability
                                , moduleName = moduleName
                                , imports = imports
                                , aspectHashes = aspectHashes
                                }
                            )
                            (Json.Decode.field "revision" (Json.Decode.map InputRevision (Json.Decode.field "key" Json.Decode.string)))
                            (Json.Decode.field "durability" durabilityDecoder)
                            (Json.Decode.field "moduleName" Json.Decode.string)
                            (Json.Decode.field "imports" (Json.Decode.list Json.Decode.string))
                            (Json.Decode.field "aspectHashes" fileAspectHashesDecoder)
                        )
                    )
                    (Json.Decode.field "bodySummary"
                        (Json.Decode.map2
                            (\revision declarations ->
                                { revision = revision
                                , declarations = declarations
                                }
                            )
                            (Json.Decode.field "revision" (Json.Decode.map InputRevision (Json.Decode.field "key" Json.Decode.string)))
                            (Json.Decode.field "declarations" (Json.Decode.list declarationDecoder))
                        )
                    )
                )
    in
    Json.Decode.decodeString (Json.Decode.list analysisDecoder |> Json.Decode.map Dict.fromList) json
        |> Result.toMaybe


encodeProjectCacheMetadata : ProjectCacheMetadata -> String
encodeProjectCacheMetadata metadata =
    Json.Encode.object
        [ ( "schemaVersion", Json.Encode.string metadata.schemaVersion )
        , ( "helperHash", Json.Encode.string metadata.helperHash )
        , ( "fileHashes"
          , metadata.fileHashes
                |> Dict.toList
                |> Json.Encode.list
                    (\( filePath, sourceHash ) ->
                        Json.Encode.object
                            [ ( "file", Json.Encode.string filePath )
                            , ( "sourceHash", Json.Encode.string sourceHash )
                            ]
                    )
          )
        ]
        |> Json.Encode.encode 0


decodeProjectCacheMetadata : String -> Maybe ProjectCacheMetadata
decodeProjectCacheMetadata json =
    let
        fileHashDecoder =
            Json.Decode.map2 Tuple.pair
                (Json.Decode.field "file" Json.Decode.string)
                (Json.Decode.field "sourceHash" Json.Decode.string)
    in
    Json.Decode.decodeString
        (Json.Decode.map3
            (\schemaVersion helperHash fileHashes ->
                { schemaVersion = schemaVersion
                , helperHash = helperHash
                , fileHashes = fileHashes
                }
            )
            (Json.Decode.field "schemaVersion" Json.Decode.string)
            (Json.Decode.field "helperHash" Json.Decode.string)
            (Json.Decode.field "fileHashes" (Json.Decode.list fileHashDecoder) |> Json.Decode.map Dict.fromList)
        )
        json
        |> Result.toMaybe


emptyRuleEvalPerf : RuleEvalPerf
emptyRuleEvalPerf =
    { moduleRuleEvalMs = 0
    , projectRuleEvalMs = 0
    , counters = Dict.empty
    }


stageMs : Time.Posix -> Time.Posix -> Int
stageMs start finish =
    Time.posixToMillis finish - Time.posixToMillis start


mergeCounterDicts : List (Dict String Int) -> Dict String Int
mergeCounterDicts dicts =
    dicts
        |> List.foldl
            (\dict acc ->
                Dict.foldl
                    (\key value innerAcc ->
                        Dict.update key
                            (\maybeExisting -> Just (value + Maybe.withDefault 0 maybeExisting))
                            innerAcc
                    )
                    acc
                    dict
            )
            Dict.empty


withTiming : String -> BackendTask FatalError a -> BackendTask FatalError (Timed a)
withTiming name work =
    Do.do BackendTask.Time.now <| \start ->
    Do.do work <| \value ->
    Do.do BackendTask.Time.now <| \finish ->
    BackendTask.succeed
        { value = value
        , stage =
            { name = name
            , ms = stageMs start finish
            }
        }


deferTask : (() -> BackendTask FatalError a) -> BackendTask FatalError a
deferTask thunk =
    BackendTask.succeed ()
        |> BackendTask.andThen (\_ -> thunk ())


cacheDecisionToString : CacheDecision -> String
cacheDecisionToString decision =
    case decision of
        FullCacheHit _ ->
            "full_hit"

        PartialMiss _ ->
            "partial_miss"

        ColdMiss _ ->
            "cold_miss"


boolToInt : Bool -> Int
boolToInt value =
    if value then
        1

    else
        0


fileExists : String -> BackendTask FatalError Bool
fileExists path =
    Glob.fromStringWithOptions
        (let
            options : Glob.Options
            options =
                Glob.defaultOptions
         in
         { options | include = Glob.OnlyFiles }
        )
        path
        |> BackendTask.map (not << List.isEmpty)


memoStatsCounters : String -> MemoRuntime.MemoStats -> Dict String Int
memoStatsCounters prefix memoStats =
    let
        baseCounters =
            Dict.fromList
                [ (prefix ++ ".lookups", memoStats.lookups)
                , (prefix ++ ".hits", memoStats.hits)
                , (prefix ++ ".misses", memoStats.misses)
                , (prefix ++ ".stores", memoStats.stores)
                ]

        topCounters =
            MemoRuntime.topFunctionStats 5 memoStats
                |> List.concatMap
                    (\( qualifiedName, stats ) ->
                        [ (prefix ++ ".byFunction." ++ qualifiedName ++ ".lookups", stats.lookups)
                        , (prefix ++ ".byFunction." ++ qualifiedName ++ ".hits", stats.hits)
                        , (prefix ++ ".byFunction." ++ qualifiedName ++ ".misses", stats.misses)
                        , (prefix ++ ".byFunction." ++ qualifiedName ++ ".stores", stats.stores)
                        ]
                    )
                |> Dict.fromList
    in
    mergeCounterDicts [ baseCounters, topCounters ]


mergeMemoStats : MemoRuntime.MemoStats -> MemoRuntime.MemoStats -> MemoRuntime.MemoStats
mergeMemoStats left right =
    { enabled = left.enabled || right.enabled
    , lookups = left.lookups + right.lookups
    , hits = left.hits + right.hits
    , misses = left.misses + right.misses
    , stores = left.stores + right.stores
    , byFunction =
        if left.lookups >= right.lookups then
            left.byFunction

        else
            right.byFunction
    }


errorCounters : List ReviewError -> Dict String Int
errorCounters errors =
    Dict.fromList
        [ ( "errors.total", List.length errors )
        , ( "errors.files", countFiles errors )
        ]


reviewAppHashFromPreparedConfig : Config -> String
reviewAppHashFromPreparedConfig config =
    Path.toString config.buildDirectory
        |> String.split "/review-app-"
        |> List.reverse
        |> List.head
        |> Maybe.withDefault "unknown"


perfStageEncoder : PerfStage -> Json.Encode.Value
perfStageEncoder stage =
    Json.Encode.object
        [ ( "name", Json.Encode.string stage.name )
        , ( "ms", Json.Encode.int stage.ms )
        ]


encodePerfTrace : PerfTrace -> String
encodePerfTrace trace =
    Json.Encode.object
        [ ( "schemaVersion", Json.Encode.string trace.schemaVersion )
        , ( "reviewAppHash", Json.Encode.string trace.reviewAppHash )
        , ( "cacheDecision", Json.Encode.string trace.cacheDecision )
        , ( "counters"
          , trace.counters
                |> Dict.toList
                |> Json.Encode.object
                    << List.map (\( key, value ) -> ( key, Json.Encode.int value ))
          )
        , ( "stages", Json.Encode.list perfStageEncoder trace.stages )
        ]
        |> Json.Encode.encode 2


writePerfTrace : Maybe String -> PerfTrace -> BackendTask FatalError ()
writePerfTrace maybePath trace =
    case maybePath of
        Just path ->
            Script.writeFile
                { path = path
                , body = encodePerfTrace trace
                }
                |> BackendTask.allowFatal

        Nothing ->
            BackendTask.succeed ()


projectCacheMetadataFor : String -> List AnalyzedTargetFile -> ProjectCacheMetadata
projectCacheMetadataFor helperHash files =
    { schemaVersion = cacheSchemaVersion
    , helperHash = helperHash
    , fileHashes =
        files
            |> List.map (\file -> ( file.path, file.analysis.sourceHash ))
            |> Dict.fromList
    }


projectCacheShapeMatches : ProjectCacheMetadata -> ProjectCacheMetadata -> Bool
projectCacheShapeMatches expected actual =
    expected.schemaVersion == actual.schemaVersion
        && expected.helperHash == actual.helperHash
        && (expected.fileHashes |> Dict.keys |> Set.fromList)
        == (actual.fileHashes |> Dict.keys |> Set.fromList)


persistProjectCacheMetadata : String -> ProjectCacheMetadata -> BackendTask FatalError ()
persistProjectCacheMetadata buildDir metadata =
    Script.writeFile
        { path = buildDir ++ "/project-cache-meta.json"
        , body = encodeProjectCacheMetadata metadata
        }
        |> BackendTask.allowFatal


loadProjectCache : String -> ProjectCacheMetadata -> BackendTask FatalError (Maybe Types.Value)
loadProjectCache buildDir expectedMetadata =
    let
        metadataPath =
            buildDir ++ "/project-cache-meta.json"

        projectPath =
            buildDir ++ "/project-cache.json"
    in
    File.rawFile metadataPath
        |> BackendTask.toResult
        |> BackendTask.andThen
            (\metadataResult ->
                case metadataResult of
                    Ok metadataJson ->
                        case decodeProjectCacheMetadata metadataJson of
                            Just actualMetadata ->
                                if projectCacheShapeMatches expectedMetadata actualMetadata then
                                    File.rawFile projectPath
                                        |> BackendTask.toResult
                                        |> BackendTask.map
                                            (\projectResult ->
                                                case projectResult of
                                                    Ok projectJson ->
                                                        ValueCodec.decodeValue projectJson

                                                    Err _ ->
                                                        Nothing
                                            )

                                else
                                    BackendTask.succeed Nothing

                            Nothing ->
                                BackendTask.succeed Nothing

                    Err _ ->
                        BackendTask.succeed Nothing
            )


analyzeSourceFile : String -> Maybe FileAnalysis
analyzeSourceFile source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            let
                moduleName =
                    moduleNameFromFile file

                imports =
                    importsFromFile file

                aspectHashes =
                    SemanticHash.computeAspectHashesFromFile file

                declarations =
                    getDeclarationHashesFromFile file
            in
            Just
                { sourceHash = hashSourceContents source
                , parsedAst =
                    { astJson =
                        Elm.Syntax.File.encode file
                            |> Json.Encode.encode 0
                    }
                , moduleSummary =
                    { revision = { key = moduleSummaryRevisionKey moduleName imports aspectHashes }
                    , durability = WorkspaceDurability
                    , moduleName = moduleName
                    , imports = imports
                    , aspectHashes = aspectHashes
                    }
                , bodySummary =
                    { revision = { key = bodySummaryRevisionKey declarations }
                    , declarations = declarations
                    }
                }

        Err _ ->
            Nothing


persistFileAnalysisCache : String -> Dict String FileAnalysis -> BackendTask FatalError ()
persistFileAnalysisCache path cache =
    Script.writeFile
        { path = path
        , body = encodeFileAnalysisCache cache
        }
        |> BackendTask.allowFatal


loadAnalyzedTargetFiles : Config -> List String -> BackendTask FatalError AnalyzedTargetFilesResult
loadAnalyzedTargetFiles config files =
    let
        cachePath =
            Path.toString config.buildDirectory ++ "/review-file-analysis-cache.json"
    in
    Do.do
        (File.rawFile cachePath
            |> BackendTask.toResult
        )
    <| \cacheFileResult ->
    let
        previousCache =
            cacheFileResult
                |> Result.toMaybe
                |> Maybe.andThen decodeFileAnalysisCache
                |> Maybe.withDefault Dict.empty

        cacheLoadBytes =
            cacheFileResult
                |> Result.map String.length
                |> Result.withDefault 0
    in
    Do.do (withTiming "read_target_files" (readTargetFiles files)) <| \readResult ->
    let
        targetFileContents =
            readResult.value
    in
    Do.do BackendTask.Time.now <| \analysisStart ->
    let
        currentPaths =
            targetFileContents |> List.map .path |> Set.fromList

        cachePathsChanged =
            (previousCache |> Dict.keys |> Set.fromList) /= currentPaths

        analyzedResults =
            targetFileContents
                |> List.map
                    (\file ->
                        let
                            currentHash =
                                hashSourceContents file.source
                        in
                        case Dict.get file.path previousCache of
                            Just cached ->
                                if cached.sourceHash == currentHash then
                                    { wasMiss = False
                                    , analyzed =
                                        Just
                                            { path = file.path
                                            , source = file.source
                                            , analysis = cached
                                            }
                                    }

                                else
                                    { wasMiss = True
                                    , analyzed =
                                        analyzeSourceFile file.source
                                            |> Maybe.map
                                                (\analysis ->
                                                    { path = file.path
                                                    , source = file.source
                                                    , analysis = analysis
                                                    }
                                                )
                                    }

                            Nothing ->
                                { wasMiss = True
                                , analyzed =
                                    analyzeSourceFile file.source
                                        |> Maybe.map
                                            (\analysis ->
                                                { path = file.path
                                                , source = file.source
                                                , analysis = analysis
                                                }
                                            )
                                }
                    )

        analyzedFiles =
            analyzedResults |> List.filterMap .analyzed

        updatedCache =
            analyzedFiles
                |> List.map (\file -> ( file.path, file.analysis ))
                |> Dict.fromList

        cacheMissed =
            analyzedResults |> List.any .wasMiss

        analysisHits =
            analyzedResults
                |> List.filter (.wasMiss >> not)
                |> List.length

        analysisMisses =
            analyzedResults
                |> List.filter .wasMiss
                |> List.length

        sourceBytes =
            targetFileContents
                |> List.map (.source >> String.length)
                |> List.sum

        astJsonBytes =
            analyzedFiles
                |> List.map (\file -> String.length file.analysis.parsedAst.astJson)
                |> List.sum

        encodedUpdatedCache =
            if cacheMissed || cachePathsChanged then
                encodeFileAnalysisCache updatedCache

            else
                ""
    in
    Do.do BackendTask.Time.now <| \analysisFinish ->
    Do.do
        (if cacheMissed || cachePathsChanged then
            Script.writeFile
                { path = cachePath
                , body = encodedUpdatedCache
                }
                |> BackendTask.allowFatal

         else
            BackendTask.succeed ()
        )
    <| \_ ->
    BackendTask.succeed
        { files = analyzedFiles
        , analysisHits = analysisHits
        , analysisMisses = analysisMisses
        , sourceBytes = sourceBytes
        , astJsonBytes = astJsonBytes
        , cacheEntries = Dict.size updatedCache
        , cacheLoadBytes = cacheLoadBytes
        , cacheStoreBytes = String.length encodedUpdatedCache
        , readMs = readResult.stage.ms
        , analysisMs = stageMs analysisStart analysisFinish
        }


{-| Parse a source file on the HOST side and encode the AST as JSON.
This avoids interpreter-side parsing entirely — the interpreter just
decodes the pre-parsed AST.

Returns Nothing if the file fails to parse.

-}
encodeFileAsJson : String -> Maybe String
encodeFileAsJson source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            Elm.Syntax.File.encode file
                |> Json.Encode.encode 0
                |> Just

        Err _ ->
            Nothing


{-| Parse a source file on the HOST side and encode the AST as binary
using Lamdera Wire3 codecs. Much more compact than JSON (~3-4x smaller).

Returns the encoded bytes as a comma-separated int list string for
embedding in an Elm expression.

-}
encodeFileAsWire : String -> Maybe { intList : String, byteCount : Int }
encodeFileAsWire source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            let
                bytes =
                    AstWireCodec.encodeToBytes file

                ints =
                    Lamdera.Wire3.intListFromBytes bytes
            in
            Just
                { intList =
                    ints
                        |> List.map String.fromInt
                        |> String.join ","
                , byteCount = Bytes.width bytes
                }

        Err _ ->
            Nothing


{-| Build expression to evaluate multiple rules by their indices in one call.
-}
buildExpressionForRules : List Int -> List { path : String, source : String, astJson : String } -> String
buildExpressionForRules ruleIndices modules =
    let
        moduleList =
            buildModuleListWithAst modules

        indexList =
            "[ " ++ (ruleIndices |> List.map String.fromInt |> String.join ", ") ++ " ]"
    in
    "ReviewRunnerHelper.runRulesByIndices " ++ indexList ++ " " ++ moduleList


buildExpressionForRulesWithModuleVar : List Int -> String -> String
buildExpressionForRulesWithModuleVar ruleIndices moduleVarName =
    let
        indexList =
            "[ " ++ (ruleIndices |> List.map String.fromInt |> String.join ", ") ++ " ]"
    in
    "ReviewRunnerHelper.runRulesByIndices " ++ indexList ++ " " ++ moduleVarName


moduleInputValue : { path : String, source : String, astJson : String } -> Types.Value
moduleInputValue moduleInput =
    Types.Record
        (FastDict.fromList
            [ ( "path", Types.String moduleInput.path )
            , ( "source", Types.String moduleInput.source )
            , ( "astJson", Types.String moduleInput.astJson )
            ]
        )


moduleInputsValue : List { path : String, source : String, astJson : String } -> Types.Value
moduleInputsValue modules =
    Types.List (List.map moduleInputValue modules)


type alias SharedModulePayloads =
    { paths : Array String
    , sources : Array String
    , astJsons : Array String
    }


emptySharedModulePayloads : SharedModulePayloads
emptySharedModulePayloads =
    { paths = Array.empty
    , sources = Array.empty
    , astJsons = Array.empty
    }


moduleHandlePayloads :
    List { path : String, source : String, astJson : String }
    -> { handlesValue : Types.Value, payloads : SharedModulePayloads }
moduleHandlePayloads modules =
    { handlesValue =
        modules
            |> List.indexedMap (\handle _ -> Types.Int handle)
            |> Types.List
    , payloads =
        { paths = modules |> List.map .path |> Array.fromList
        , sources = modules |> List.map .source |> Array.fromList
        , astJsons = modules |> List.map .astJson |> Array.fromList
        }
    }


buildExpressionForRulesWithHandleVar : List Int -> String -> String
buildExpressionForRulesWithHandleVar ruleIndices moduleHandlesVarName =
    let
        indexList =
            "[ " ++ (ruleIndices |> List.map String.fromInt |> String.join ", ") ++ " ]"
    in
    "ReviewRunnerHelper.runRulesByIndicesFromHandles " ++ indexList ++ " " ++ moduleHandlesVarName


buildExpressionForRule : Int -> List { path : String, source : String, astJson : String } -> String
buildExpressionForRule ruleIndex modules =
    let
        moduleRecords =
            modules
                |> List.map
                    (\{ path, source, astJson } ->
                        "{ path = \""
                            ++ escapeElmString path
                            ++ "\", source = \""
                            ++ escapeElmString source
                            ++ "\", astJson = \""
                            ++ escapeElmString astJson
                            ++ "\" }"
                    )
                |> String.join ", "

        moduleList =
            case modules of
                [] ->
                    "[]"

                _ ->
                    "[ " ++ moduleRecords ++ " ]"
    in
    "ReviewRunnerHelper.runSingleRule " ++ String.fromInt ruleIndex ++ " " ++ moduleList


ruleCacheValueKey : String -> Int -> String
ruleCacheValueKey ruleName ruleId =
    ruleName ++ "-" ++ String.fromInt ruleId


{-| Build an expression that passes pre-parsed AST JSON to the interpreter.
Each module is encoded as `{ path : String, ast : String }` where `ast`
is the JSON-encoded elm-syntax File.
-}
buildExpressionWithAst : List { path : String, source : String, astJson : String } -> String
buildExpressionWithAst modules =
    let
        moduleRecords =
            modules
                |> List.map
                    (\{ path, source, astJson } ->
                        "{ path = \""
                            ++ escapeElmString path
                            ++ "\", source = \""
                            ++ escapeElmString source
                            ++ "\", astJson = \""
                            ++ escapeElmString astJson
                            ++ "\" }"
                    )
                |> String.join ", "

        moduleList =
            case modules of
                [] ->
                    "[]"

                _ ->
                    "[ " ++ moduleRecords ++ " ]"
    in
    "ReviewRunnerHelper.runReview " ++ moduleList


{-| Build an expression using Wire3 binary-encoded ASTs (most compact).
Each module carries path, source, and a `List Int` (Wire3 bytes).
-}
buildExpressionWithWire : List { path : String, source : String, wireIntList : String } -> String
buildExpressionWithWire modules =
    let
        moduleRecords =
            modules
                |> List.map
                    (\{ path, source, wireIntList } ->
                        "{ path = \""
                            ++ escapeElmString path
                            ++ "\", source = \""
                            ++ escapeElmString source
                            ++ "\", wireBytes = [" ++ wireIntList ++ "] }"
                    )
                |> String.join ", "

        moduleList =
            case modules of
                [] ->
                    "[]"

                _ ->
                    "[ " ++ moduleRecords ++ " ]"
    in
    "ReviewRunnerHelper.runReview " ++ moduleList


{-| The inline ReviewRunnerHelper module source, injected as a source override.
-}
reviewRunnerHelperSource : String
reviewRunnerHelperSource =
    String.join "\n"
        [ "module ReviewRunnerHelper exposing (buildProject, buildProjectFromHandles, extractRuleCaches, ruleCount, ruleNames, runReview, runReviewCaching, runReviewCachingByIndices, runReviewCachingWithProject, runReviewWithCachedProject, runReviewWithCachedRules, runRulesByIndices, runRulesByIndicesFromHandles, runSingleRule, runTwoRuleGroups)"
        , "import Json.Decode"
        , "import Elm.Syntax.File"
        , "import Review.Project as Project"
        , "import Review.Rule as Rule"
        , "import ReviewConfig"
        , ""
        , "buildProject modules ="
        , "    let"
        , "        addParsed mod proj ="
        , "            case Json.Decode.decodeString Elm.Syntax.File.decoder mod.astJson of"
        , "                Ok ast -> Project.addParsedModule { path = mod.path, source = mod.source, ast = ast } proj"
        , "                Err _ -> Project.addModule { path = mod.path, source = mod.source } proj"
        , "    in"
        , "    List.foldl addParsed Project.new modules"
        , ""
        , "buildProjectFromHandles moduleHandles ="
        , "    let"
        , "        addParsed handle proj ="
        , "            let"
        , "                path = moduleHandlePathMarker handle"
        , "                source = moduleHandleSourceMarker handle"
        , "                astJson = moduleHandleAstJsonMarker handle"
        , "            in"
        , "            case Json.Decode.decodeString Elm.Syntax.File.decoder astJson of"
        , "                Ok ast -> Project.addParsedModule { path = path, source = source, ast = ast } proj"
        , "                Err _ -> Project.addModule { path = path, source = source } proj"
        , "    in"
        , "    List.foldl addParsed Project.new moduleHandles"
        , ""
        , "moduleHandlePathMarker handle ="
        , "    \"\""
        , ""
        , "moduleHandleSourceMarker handle ="
        , "    \"\""
        , ""
        , "moduleHandleAstJsonMarker handle ="
        , "    \"\""
        , ""
        , "runReviewCachingWithProject indices modules ="
        , "    let"
        , "        selectedRules = indices |> List.filterMap (\\i -> List.head (List.drop i ReviewConfig.config))"
        , "        project = buildProject modules |> projectCacheMarker"
        , "        ( errors, updatedRules ) = Rule.review selectedRules project"
        , "        errorStr = errors |> List.map formatError |> String.join \"\\n\""
        , "    in"
        , "    ( errorStr, updatedRules )"
        , ""
        , "extractRuleCaches updatedRules ="
        , "    let"
        , "        -- Run Rule.review on empty project to trigger initialCacheMarker"
        , "        -- for each rule with its warm cache. The intercept yields them."
        , "        ( _, _ ) = Rule.review updatedRules Project.new"
        , "    in"
        , "    \"done\""
        , ""
        , "runReviewWithCachedProject indices cachedProj modules ="
        , "    let"
        , "        selectedRules = indices |> List.filterMap (\\i -> List.head (List.drop i ReviewConfig.config))"
        , "        -- Use cached project, but add the changed module on top"
        , "        project = List.foldl"
        , "            (\\mod proj ->"
        , "                case Json.Decode.decodeString Elm.Syntax.File.decoder mod.astJson of"
        , "                    Ok ast -> Project.addParsedModule { path = mod.path, source = mod.source, ast = ast } proj"
        , "                    Err _ -> Project.addModule { path = mod.path, source = mod.source } proj"
        , "            )"
        , "            cachedProj"
        , "            modules"
        , "            |> projectCacheMarker"
        , "        ( errors, updatedRules ) = Rule.review selectedRules project"
        , "        errorStr = errors |> List.map formatError |> String.join \"\\n\""
        , "    in"
        , "    ( errorStr, updatedRules )"
        , ""
        , "projectCacheMarker project = project"
        , ""
        , "runReview modules ="
        , "    let"
        , "        project = buildProject modules"
        , "        ( errors, _ ) = Rule.review ReviewConfig.config project"
        , "    in"
        , "    errors |> List.map formatError |> String.join \"\\n\""
        , ""
        , "runReviewCaching modules ="
        , "    let"
        , "        project = buildProject modules"
        , "        ( errors, updatedRules ) = Rule.review ReviewConfig.config project"
        , "        errorStr = errors |> List.map formatError |> String.join \"\\n\""
        , "    in"
        , "    ( errorStr, updatedRules )"
        , ""
        , "runReviewWithCachedRules cachedRules modules ="
        , "    let"
        , "        project = buildProject modules"
        , "        ( errors, updatedRules ) = Rule.review cachedRules project"
        , "        errorStr = errors |> List.map formatError |> String.join \"\\n\""
        , "    in"
        , "    ( errorStr, updatedRules )"
        , ""
        , "runReviewCachingByIndices indices modules ="
        , "    let"
        , "        selectedRules = indices |> List.filterMap (\\i -> List.head (List.drop i ReviewConfig.config))"
        , "        project = buildProject modules"
        , "        ( errors, updatedRules ) = Rule.review selectedRules project"
        , "        errorStr = errors |> List.map formatError |> String.join \"\\n\""
        , "    in"
        , "    ( errorStr, updatedRules )"
        , ""
        , "runRulesByIndices indices modules ="
        , "    let"
        , "        selectedRules = indices |> List.filterMap (\\i -> List.head (List.drop i ReviewConfig.config))"
        , "        project = buildProject modules"
        , "        ( errors, _ ) = Rule.review selectedRules project"
        , "    in"
        , "    errors |> List.map formatError |> String.join \"\\n\""
        , ""
        , "runRulesByIndicesFromHandles indices moduleHandles ="
        , "    let"
        , "        selectedRules = indices |> List.filterMap (\\i -> List.head (List.drop i ReviewConfig.config))"
        , "        project = buildProjectFromHandles moduleHandles"
        , "        ( errors, _ ) = Rule.review selectedRules project"
        , "    in"
        , "    errors |> List.map formatError |> String.join \"\\n\""
        , ""
        , "runSingleRule ruleIndex modules ="
        , "    case List.head (List.drop ruleIndex ReviewConfig.config) of"
        , "        Just rule ->"
        , "            let"
        , "                project = buildProject modules"
        , "                ( errors, _ ) = Rule.review [ rule ] project"
        , "            in"
        , "            errors |> List.map formatError |> String.join \"\\n\""
        , "        Nothing -> \"\""
        , ""
        , "runTwoRuleGroups firstIndices firstModules secondIndices secondModules ="
        , "    ( runRulesByIndices firstIndices firstModules"
        , "    , runRulesByIndices secondIndices secondModules"
        , "    )"
        , ""
        , "ruleCount = List.length ReviewConfig.config"
        , ""
        , "ruleNames = ReviewConfig.config |> List.map Rule.ruleName |> String.join \",\""
        , ""
        , "formatError err ="
        , "    let"
        , "        range = Rule.errorRange err"
        , "    in"
        , "    String.join \"|\""
        , "        [ \"RULE:\" ++ Rule.errorRuleName err"
        , "        , \"FILE:\" ++ Rule.errorFilePath err"
        , "        , \"LINE:\" ++ String.fromInt range.start.row"
        , "        , \"COL:\" ++ String.fromInt range.start.column"
        , "        , \"MSG:\" ++ Rule.errorMessage err"
        , "        ]"
        ]



-- SCRIPT IMPLEMENTATION


task : Config -> BackendTask FatalError ()
task config =
    Do.do (withTiming "prepare_config" (prepareConfig config)) <| \preparedConfigTimed ->
    let
        preparedConfig =
            preparedConfigTimed.value

        declCachePath =
            Path.toString preparedConfig.buildDirectory ++ "/review-decl-cache.json"

        reviewAppHashValue =
            reviewAppHashFromPreparedConfig preparedConfig
    in
    -- Load previous per-declaration cache from disk
    Do.do
        (withTiming "load_decl_cache"
            (File.rawFile declCachePath
                |> BackendTask.toResult
                |> BackendTask.map
                    (\result ->
                        { cache =
                            result
                                |> Result.toMaybe
                                |> Maybe.andThen decodeCacheState
                                |> Maybe.withDefault Dict.empty
                        , bytes =
                            result
                                |> Result.map String.length
                                |> Result.withDefault 0
                        }
                    )
                )
        )
    <| \declCacheTimed ->
    let
        previousCache =
            declCacheTimed.value.cache
    in
    Do.do (withTiming "resolve_target_files" (resolveTargetFiles preparedConfig)) <| \targetFilesTimed ->
    let
        targetFiles =
            targetFilesTimed.value
    in
    Do.do (loadAnalyzedTargetFiles preparedConfig targetFiles) <| \analyzedTargetFiles ->
    let
        targetFileContents =
            analyzedTargetFiles.files

        decision =
            checkCacheWithAnalyses previousCache targetFileContents

        baseStages =
            [ preparedConfigTimed.stage
            , declCacheTimed.stage
            , targetFilesTimed.stage
            , { name = "read_target_files", ms = analyzedTargetFiles.readMs }
            , { name = "analyze_target_files", ms = analyzedTargetFiles.analysisMs }
            ]

        baseCounters =
            Dict.fromList
                [ ( "target.files", List.length targetFileContents )
                , ( "decl_cache.entries", Dict.size previousCache )
                , ( "decl_cache.loaded_bytes", declCacheTimed.value.bytes )
                , ( "analysis_cache.entries", analyzedTargetFiles.cacheEntries )
                , ( "analysis_cache.loaded_bytes", analyzedTargetFiles.cacheLoadBytes )
                , ( "analysis_cache.stored_bytes", analyzedTargetFiles.cacheStoreBytes )
                , ( "analysis_cache.hits", analyzedTargetFiles.analysisHits )
                , ( "analysis_cache.misses", analyzedTargetFiles.analysisMisses )
                , ( "source.bytes", analyzedTargetFiles.sourceBytes )
                , ( "ast_json.bytes", analyzedTargetFiles.astJsonBytes )
                ]

        finishWithTrace errors extraStages extraCounters =
            Do.do
                (writePerfTrace
                    preparedConfig.perfTraceJson
                    { schemaVersion = cacheSchemaVersion
                    , reviewAppHash = reviewAppHashValue
                    , cacheDecision = cacheDecisionToString decision
                    , counters = mergeCounterDicts [ baseCounters, extraCounters, errorCounters errors ]
                    , stages = baseStages ++ extraStages
                    }
                )
            <| \_ ->
            reportErrors errors
    in
    case decision of
        FullCacheHit errors ->
            -- All declarations cached — skip interpreter entirely
            finishWithTrace
                errors
                []
                (Dict.fromList
                    [ ( "stale.files", 0 )
                    , ( "decl_cache.stored_bytes", 0 )
                    ]
                )

        ColdMiss _ ->
            -- No cache — treat all files as stale (same code path as PartialMiss)
            Do.do (withTiming "load_review_project" (loadReviewProjectDetailed preparedConfig)) <| \reviewProjectTimed ->
            let
                reviewProject =
                    reviewProjectTimed.value.project
            in
            Do.do (withTiming "get_rule_info" (getRuleInfoWithProject preparedConfig reviewProject)) <| \ruleInfoTimed ->
            let
                ruleInfo =
                    ruleInfoTimed.value
            in
            Do.do (loadAndEvalHybridPartialWithProject preparedConfig reviewProject ruleInfo targetFileContents targetFileContents []) <| \runResult ->
            let
                errors =
                    parseReviewOutput runResult.output

                newCache =
                    updateCacheWithAnalyses Dict.empty targetFileContents errors

                encodedDeclCache =
                    encodeCacheState newCache
            in
            Do.do
                (withTiming "persist_decl_cache"
                    (Script.writeFile
                        { path = declCachePath
                        , body = encodedDeclCache
                        }
                        |> BackendTask.allowFatal
                    )
                )
            <| \persistDeclCacheTimed ->
            finishWithTrace
                errors
                [ reviewProjectTimed.stage
                , ruleInfoTimed.stage
                , { name = "module_rule_eval", ms = runResult.perf.moduleRuleEvalMs }
                , { name = "project_rule_eval", ms = runResult.perf.projectRuleEvalMs }
                , persistDeclCacheTimed.stage
                ]
                (mergeCounterDicts
                    [ reviewProjectTimed.value.counters
                    , runResult.perf.counters
                    , Dict.fromList
                        [ ( "stale.files", List.length targetFileContents )
                        , ( "decl_cache.stored_bytes", String.length encodedDeclCache )
                        ]
                    ]
                )

        PartialMiss { cachedErrors, staleFiles } ->
            -- Some files changed. Module rules on unchanged files use cachedErrors.
            -- Only stale files need module rule re-eval. Project rules always re-eval.
            Do.do (withTiming "load_review_project" (loadReviewProjectDetailed preparedConfig)) <| \reviewProjectTimed ->
            let
                reviewProject =
                    reviewProjectTimed.value.project
            in
            Do.do (withTiming "get_rule_info" (getRuleInfoWithProject preparedConfig reviewProject)) <| \ruleInfoTimed ->
            let
                ruleInfo =
                    ruleInfoTimed.value

                staleFileContents =
                    targetFileContents |> List.filter (\f -> List.member f.path staleFiles)
            in
            Do.do (loadAndEvalHybridPartialWithProject preparedConfig reviewProject ruleInfo targetFileContents staleFileContents cachedErrors) <| \runResult ->
            let
                freshErrors =
                    parseReviewOutput runResult.output

                newCache =
                    updateCacheWithAnalyses previousCache targetFileContents freshErrors

                encodedDeclCache =
                    encodeCacheState newCache
            in
            Do.do
                (withTiming "persist_decl_cache"
                    (Script.writeFile
                        { path = declCachePath
                        , body = encodedDeclCache
                        }
                        |> BackendTask.allowFatal
                    )
                )
            <| \persistDeclCacheTimed ->
            finishWithTrace
                freshErrors
                [ reviewProjectTimed.stage
                , ruleInfoTimed.stage
                , { name = "module_rule_eval", ms = runResult.perf.moduleRuleEvalMs }
                , { name = "project_rule_eval", ms = runResult.perf.projectRuleEvalMs }
                , persistDeclCacheTimed.stage
                ]
                (mergeCounterDicts
                    [ reviewProjectTimed.value.counters
                    , runResult.perf.counters
                    , Dict.fromList
                        [ ( "stale.files", List.length staleFileContents )
                        , ( "decl_cache.stored_bytes", String.length encodedDeclCache )
                        ]
                    ]
                )


prepareConfig : Config -> BackendTask FatalError Config
prepareConfig config =
    Do.do (reviewAppHash config) <| \appHash ->
    let
        preparedConfig =
            { config
                | buildDirectory =
                    Path.path (Path.toString config.buildDirectory ++ "/review-app-" ++ appHash)
            }
    in
    Do.do (Script.exec "mkdir" [ "-p", Path.toString preparedConfig.buildDirectory ]) <| \_ ->
    Do.do (ensureReviewDepsCached preparedConfig) <| \_ ->
    BackendTask.succeed preparedConfig


reviewAppHash : Config -> BackendTask FatalError String
reviewAppHash config =
    Do.do (reviewAppFiles config) <| \files ->
    Do.do
        (files
            |> List.map
                (\path ->
                    File.rawFile path
                        |> BackendTask.allowFatal
                        |> BackendTask.map (\contents -> path ++ "\n" ++ contents)
                )
            |> BackendTask.Extra.combine
        )
    <| \fileContents ->
    ([ cacheSchemaVersion
     , reviewRunnerHelperSource
     ]
        ++ fileContents
    )
        |> String.join "\u{001F}"
        |> FNV1a.hash
        |> String.fromInt
        |> BackendTask.succeed


reviewAppFiles : Config -> BackendTask FatalError (List String)
reviewAppFiles config =
    Glob.fromStringWithOptions
        (let
            o =
                Glob.defaultOptions
         in
         { o | include = Glob.OnlyFiles }
        )
        (config.reviewDir ++ "/src/**/*.elm")
        |> BackendTask.toResult
        |> BackendTask.map
            (\result ->
                case result of
                    Ok files ->
                        (config.reviewDir ++ "/elm.json")
                            :: List.sort files

                    Err _ ->
                        [ config.reviewDir ++ "/elm.json" ]
            )


ensureReviewDepsCached : Config -> BackendTask FatalError ()
ensureReviewDepsCached config =
    let
        stampPath =
            Path.toString config.buildDirectory ++ "/review-deps-ready"
    in
    File.rawFile stampPath
        |> BackendTask.toResult
        |> BackendTask.andThen
            (\result ->
                case result of
                    Ok _ ->
                        BackendTask.succeed ()

                    Err _ ->
                        Do.do (ensureReviewDeps config.reviewDir) <| \depsReady ->
                        if depsReady then
                            Script.writeFile
                                { path = stampPath
                                , body = cacheSchemaVersion
                                }
                                |> BackendTask.allowFatal

                        else
                            BackendTask.succeed ()
            )


{-| Load the review project and evaluate Rule.review via the interpreter.

Uses `prepareAndEvalRaw` with `runReviewCaching` to get both errors AND
the updated rules Value (with elm-review's internal per-module cache).
Returns the rules Value so the caller can pass it back on subsequent runs.
-}
loadAndEval :
    Config
    -> List { path : String, source : String }
    -> BackendTask FatalError { output : String, reviewProject : InterpreterProject }
loadAndEval config targetFileContents =
    Do.do (loadReviewProject config) <| \reviewProject ->
    let
        modulesWithAst =
            targetFileContents
                |> List.filterMap
                    (\{ path, source } ->
                        encodeFileAsJson source
                            |> Maybe.map (\astJson -> { path = path, source = source, astJson = astJson })
                    )

        expression =
            buildExpressionWithAst modulesWithAst
    in
    Cache.run { jobs = Nothing } config.buildDirectory
        (InterpreterProject.evalWithSourceOverrides reviewProject
            { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
            , expression = expression
            , sourceOverrides = [ reviewRunnerHelperSource ]
            }
            Cache.succeed
        )
        |> BackendTask.andThen
            (\cacheResult ->
                File.rawFile (Path.toString cacheResult.output)
                    |> BackendTask.allowFatal
                    |> BackendTask.map (\output -> { output = output, reviewProject = reviewProject })
            )


{-| Two-pass evaluation: first pass warms elm-review's internal rule cache,
second pass uses cached rules with a simulated file change.

This is the proof-of-concept for Salsa-style rule cache preservation.
Measures the time difference between fresh eval and cached-rule eval.
-}
loadAndEvalTwoPass :
    Config
    -> List { path : String, source : String }
    -> BackendTask FatalError { output : String, reviewProject : InterpreterProject }
loadAndEvalTwoPass config targetFileContents =
    Do.do (loadReviewProject config) <| \reviewProject ->
    let
        modulesWithAst =
            targetFileContents
                |> List.filterMap
                    (\{ path, source } ->
                        encodeFileAsJson source
                            |> Maybe.map (\astJson -> { path = path, source = source, astJson = astJson })
                    )

        moduleList =
            buildModuleListWithAst modulesWithAst

        -- Expression that returns (errorString, updatedRules) tuple
        cachingExpression =
            "ReviewRunnerHelper.runReviewCaching " ++ moduleList
    in
    -- PASS 1: Fresh evaluation, get (errors, rules) tuple
    Do.do BackendTask.Time.now <| \t1 ->
    let
        pass1Result =
            InterpreterProject.prepareAndEvalRaw reviewProject
                { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                , expression = cachingExpression
                , sourceOverrides = [ reviewRunnerHelperSource ]
                }
    in
    case pass1Result of
        Err errStr ->
            Do.do (Script.log ("Pass 1 error: " ++ String.left 200 errStr)) <| \_ ->
            BackendTask.succeed { output = errStr, reviewProject = reviewProject }

        Ok (Types.Tuple (Types.String errorStr) rulesValue) ->
            Do.do BackendTask.Time.now <| \t2 ->
            Do.do (Script.log ("Pass 1 (fresh): " ++ String.fromInt (Time.posixToMillis t2 - Time.posixToMillis t1) ++ "ms, " ++ String.fromInt (List.length (parseReviewOutput errorStr)) ++ " errors")) <| \_ ->
            -- PASS 2: Simulated file change — modify one file, use cached rules
            let
                -- Simulate a change to the first file by appending a declaration
                changedModulesWithAst =
                    case modulesWithAst of
                        first :: rest ->
                            let
                                changedSource =
                                    first.source ++ "\n\ntestBenchmarkInjected__ = 42\n"

                                changedAstJson =
                                    encodeFileAsJson changedSource
                                        |> Maybe.withDefault first.astJson
                            in
                            { first | source = changedSource, astJson = changedAstJson } :: rest

                        [] ->
                            []

                changedModuleList =
                    buildModuleListWithAst changedModulesWithAst

                -- Expression using cached rules
                cachedExpression =
                    "ReviewRunnerHelper.runReviewWithCachedRules cachedRules__ " ++ changedModuleList

                injectedValues =
                    FastDict.singleton "cachedRules__" rulesValue
            in
            Do.do BackendTask.Time.now <| \t3 ->
            let
                pass2Result =
                    InterpreterProject.prepareAndEvalWithValues reviewProject
                        { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                        , expression = cachedExpression
                        , sourceOverrides = [ reviewRunnerHelperSource ]
                        , injectedValues = injectedValues
                        }
            in
            case pass2Result of
                Ok (Types.Tuple (Types.String errorStr2) _) ->
                    Do.do BackendTask.Time.now <| \t4 ->
                    Do.do (Script.log ("Pass 2 (cached rules, 1 file changed): " ++ String.fromInt (Time.posixToMillis t4 - Time.posixToMillis t3) ++ "ms, " ++ String.fromInt (List.length (parseReviewOutput errorStr2)) ++ " errors")) <| \_ ->
                    BackendTask.succeed { output = errorStr, reviewProject = reviewProject }

                Ok (Types.String s) ->
                    -- runReviewWithCachedRules returned string directly (error?)
                    Do.do BackendTask.Time.now <| \t4 ->
                    Do.do (Script.log ("Pass 2 returned String: " ++ String.left 100 s ++ " (" ++ String.fromInt (Time.posixToMillis t4 - Time.posixToMillis t3) ++ "ms)")) <| \_ ->
                    BackendTask.succeed { output = errorStr, reviewProject = reviewProject }

                Ok other ->
                    Do.do (Script.log ("Pass 2 unexpected value type")) <| \_ ->
                    BackendTask.succeed { output = errorStr, reviewProject = reviewProject }

                Err errStr2 ->
                    Do.do (Script.log ("Pass 2 error: " ++ String.left 200 errStr2)) <| \_ ->
                    BackendTask.succeed { output = errorStr, reviewProject = reviewProject }

        Ok _ ->
            Do.do (Script.log "Pass 1: unexpected return type (expected Tuple)") <| \_ ->
            -- Fallback: run normal eval
            loadAndEval config targetFileContents


{-| Build the module list literal for expressions with AST JSON.
-}
buildModuleListWithAst : List { path : String, source : String, astJson : String } -> String
buildModuleListWithAst modules =
    let
        moduleRecords =
            modules
                |> List.map
                    (\{ path, source, astJson } ->
                        "{ path = \""
                            ++ escapeElmString path
                            ++ "\", source = \""
                            ++ escapeElmString source
                            ++ "\", astJson = \""
                            ++ escapeElmString astJson
                            ++ "\" }"
                    )
                |> String.join ", "
    in
    case modules of
        [] ->
            "[]"

        _ ->
            "[ " ++ moduleRecords ++ " ]"


{-| Partial hybrid evaluation for PartialMiss:
Module rules on STALE files only (unchanged files use cachedErrors).
Project rules re-eval on full project.
-}
loadAndEvalHybridPartial :
    Config
    -> List { index : Int, name : String, ruleType : RuleType }
    -> List AnalyzedTargetFile
    -> List AnalyzedTargetFile
    -> List ReviewError
    -> BackendTask FatalError ReviewRunResult
loadAndEvalHybridPartial config ruleInfo allFileContents staleFileContents cachedErrors =
    Do.do (loadReviewProject config) <| \reviewProject ->
    loadAndEvalHybridPartialWithProject config reviewProject ruleInfo allFileContents staleFileContents cachedErrors


loadAndEvalHybridPartialWithProject :
    Config
    -> InterpreterProject
    -> List { index : Int, name : String, ruleType : RuleType }
    -> List AnalyzedTargetFile
    -> List AnalyzedTargetFile
    -> List ReviewError
    -> BackendTask FatalError ReviewRunResult
loadAndEvalHybridPartialWithProject config reviewProject ruleInfo allFileContents staleFileContents cachedErrors =
    let
        helperHash =
            FNV1a.hash reviewRunnerHelperSource |> String.fromInt

        projectSemanticKey =
            computeSemanticKeyFromAnalyses allFileContents

        staleModulesWithAst =
            staleFileContents
                |> List.map
                    (\file ->
                        { path = file.path
                        , source = file.source
                        , astJson = file.analysis.parsedAst.astJson
                        }
                    )

        allModulesWithAst =
            allFileContents
                |> List.map
                    (\file ->
                        { path = file.path
                        , source = file.source
                        , astJson = file.analysis.parsedAst.astJson
                        }
                    )

        -- Per-file aspect hashes for narrow project rule keys
        allFileAspectHashes : Dict String SemanticHash.FileAspectHashes
        allFileAspectHashes =
            allFileContents
                |> List.map
                    (\file ->
                        ( file.path, file.analysis.moduleSummary.aspectHashes )
                    )
                |> Dict.fromList

        depGraph : DepGraph.Graph
        depGraph =
            DepGraph.buildGraphFromModuleData
                (allFileContents
                    |> List.map
                        (\file ->
                            { filePath = file.path
                            , moduleName = file.analysis.moduleSummary.moduleName
                            , imports = file.analysis.moduleSummary.imports
                            }
                        )
                )

        moduleRules =
            ruleInfo |> List.filter (\r -> r.ruleType == ModuleRule)

        projectRules =
            ruleInfo |> List.filter (\r -> r.ruleType == ProjectRule)

        stalePaths =
            staleFileContents |> List.map .path |> Set.fromList

        -- Cached module rule errors for UNCHANGED files
        cachedModuleRuleErrors =
            cachedErrors
                |> List.filter
                    (\err ->
                        not (Set.member err.filePath stalePaths)
                            && List.any (\r -> r.name == err.ruleName) moduleRules
                    )
                |> List.map formatErrorLine
                |> String.join "\n"

        moduleRuleIndices =
            moduleRules |> List.map .index

        moduleRuleProfile =
            mergeProfiles NoCrossModule moduleRules

        -- Module rules: batch all module rules for each STALE file
        moduleRuleMonads =
            if List.isEmpty moduleRules then
                []

            else
                staleFileContents
                    |> List.indexedMap
                        (\fileIdx file ->
                            let
                                fileHashes =
                                    Dict.get file.path allFileAspectHashes
                                        |> Maybe.withDefault file.analysis.moduleSummary.aspectHashes

                                moduleWithAst =
                                    { path = file.path
                                    , source = file.source
                                    , astJson = file.analysis.parsedAst.astJson
                                    }

                                fileNarrowKey =
                                    narrowCacheKey moduleRuleProfile file.path fileHashes allFileAspectHashes depGraph

                                cacheKey =
                                    "smr|" ++ helperHash ++ "|" ++ (moduleRuleIndices |> List.map String.fromInt |> String.join ",") ++ "|" ++ file.path ++ "|" ++ fileNarrowKey
                            in
                            Cache.do (Cache.writeFile cacheKey Cache.succeed) <| \keyHash ->
                            Cache.compute [ "smr", file.path ]
                                keyHash
                                (\() ->
                                    case
                                        InterpreterProject.prepareAndEval reviewProject
                                            { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                                            , expression = buildExpressionForRules moduleRuleIndices [ moduleWithAst ]
                                            , sourceOverrides = [ reviewRunnerHelperSource ]
                                            }
                                    of
                                        Ok s ->
                                            s

                                        Err e ->
                                            e
                                )
                            <| \hash ->
                            Cache.succeed
                                { filename = Path.path ("smr-" ++ String.fromInt fileIdx)
                                , hash = hash
                                }
                        )

        allMonads =
            moduleRuleMonads
                |> Cache.sequence
                |> Cache.andThen Cache.combine

        basePerfCounters =
            Dict.fromList
                [ ( "rules.module.count", List.length moduleRules )
                , ( "rules.project.count", List.length projectRules )
                ]
    in
    Do.do
        (withTiming "module_rule_eval"
            (Cache.run { jobs = Nothing } config.buildDirectory allMonads
                |> BackendTask.andThen
                    (\cacheResult ->
                        let
                            mrFiles =
                                if List.isEmpty moduleRules then
                                    []

                                else
                                    staleModulesWithAst
                                        |> List.indexedMap
                                            (\fileIdx _ ->
                                                File.rawFile
                                                    (Path.toString cacheResult.output ++ "/smr-" ++ String.fromInt fileIdx)
                                                    |> BackendTask.allowFatal
                                            )
                        in
                        mrFiles
                            |> BackendTask.Extra.combine
                            |> BackendTask.map
                                (\staleOutputs ->
                                    let
                                        freshModuleErrors =
                                            staleOutputs
                                                |> List.filter (\s -> not (String.isEmpty (String.trim s)) && not (String.startsWith "ERROR:" s))
                                                |> String.join "\n"
                                    in
                                    [ cachedModuleRuleErrors, freshModuleErrors ]
                                        |> List.filter (not << String.isEmpty)
                                        |> String.join "\n"
                                )
                    )
            )
        )
    <| \modulePhaseTimed ->
    let
        moduleRuleOutput =
            modulePhaseTimed.value
    in
    Do.do
        (withTiming "project_rule_eval"
            (if List.isEmpty projectRules then
                BackendTask.succeed
                    { output = moduleRuleOutput
                    , counters = Dict.fromList [ ( "project_rules.skipped", 1 ) ]
                    }

             else
                let
                    importersAllRules =
                        projectRules
                            |> List.filter
                                (\rule ->
                                    case (profileForRule rule.name).crossModuleDep of
                                        ImportersOf ->
                                            True

                                        _ ->
                                            False
                                )

                    importersFoldOnlyRules =
                        importersAllRules
                            |> List.filter (\rule -> not (importersNeedsImporterContext rule.name))

                    importersOfRules =
                        importersAllRules
                            |> List.filter (\rule -> importersNeedsImporterContext rule.name)

                    dependenciesOfRules =
                        projectRules
                            |> List.filter
                                (\rule ->
                                    case (profileForRule rule.name).crossModuleDep of
                                        DependenciesOf ->
                                            True

                                        _ ->
                                            False
                                )

                    fullProjectRules =
                        projectRules
                            |> List.filter
                                (\rule ->
                                    case (profileForRule rule.name).crossModuleDep of
                                        FullProject ->
                                            True

                                        _ ->
                                            False
                                )

                    prImportersFoldCacheDir =
                        Path.toString config.buildDirectory ++ "/pr-importers-fold-only"

                    prCacheDir =
                        Path.toString config.buildDirectory ++ "/pr-per-file"

                    importersFoldProfile =
                        mergeProfiles ImportersOf importersFoldOnlyRules

                    importersProfile =
                        mergeProfiles ImportersOf importersOfRules

                    dependenciesProfile =
                        mergeProfiles DependenciesOf dependenciesOfRules

                    importersFoldPerFileKeys =
                        buildPerFileRuleKeys
                            "prf-iof"
                            helperHash
                            importersFoldOnlyRules
                            importersFoldProfile
                            allFileContents
                            allFileAspectHashes
                            depGraph

                    importersPerFileKeys =
                        buildPerFileRuleKeys
                            "prf-ioc"
                            helperHash
                            importersOfRules
                            importersProfile
                            allFileContents
                            allFileAspectHashes
                            depGraph

                    depsPerFileKeys =
                        buildPerFileRuleKeys
                            "prf-do"
                            helperHash
                            dependenciesOfRules
                            dependenciesProfile
                            allFileContents
                            allFileAspectHashes
                            depGraph

                    doCacheDir =
                        Path.toString config.buildDirectory ++ "/pr-deps-of"
                in
                Do.do (Script.exec "mkdir" [ "-p", prImportersFoldCacheDir ]) <| \_ ->
                Do.do (Script.exec "mkdir" [ "-p", prCacheDir ]) <| \_ ->
                Do.do (Script.exec "mkdir" [ "-p", doCacheDir ]) <| \_ ->
                Do.do
                    (withTiming "project.importers_fold.cache_check"
                        (if List.isEmpty importersFoldOnlyRules then
                            BackendTask.succeed []

                         else
                            checkPerFileRuleCache prImportersFoldCacheDir importersFoldPerFileKeys
                        )
                    )
                <| \importersFoldChecksTimed ->
                Do.do
                    (withTiming "project.importers.cache_check"
                        (if List.isEmpty importersOfRules then
                            BackendTask.succeed []

                         else
                            checkPerFileRuleCache prCacheDir importersPerFileKeys
                        )
                    )
                <| \importersChecksTimed ->
                Do.do
                    (withTiming "project.deps.cache_check"
                        (if List.isEmpty dependenciesOfRules then
                            BackendTask.succeed []

                         else
                            checkPerFileRuleCache doCacheDir depsPerFileKeys
                        )
                    )
                <| \depsChecksTimed ->
                let
                    importersFoldChecks =
                        importersFoldChecksTimed.value

                    importersChecks =
                        importersChecksTimed.value

                    depsChecks =
                        depsChecksTimed.value

                    importersFoldIndices =
                        importersFoldOnlyRules |> List.map .index

                    ioIndices =
                        importersOfRules |> List.map .index

                    doIndices =
                        dependenciesOfRules |> List.map .index

                    importersFoldAllHit =
                        List.isEmpty importersFoldOnlyRules || List.all .hit importersFoldChecks

                    importersAllHit =
                        List.isEmpty importersOfRules || List.all .hit importersChecks

                    depsAllHit =
                        List.isEmpty dependenciesOfRules || List.all .hit depsChecks

                    importersFoldMissedPaths =
                        importersFoldChecks
                            |> List.filter (not << .hit)
                            |> List.map .path

                    importersMissedPaths =
                        importersChecks
                            |> List.filter (not << .hit)
                            |> List.map .path

                    depsMissedPaths =
                        depsChecks
                            |> List.filter (not << .hit)
                            |> List.map .path

                    importersFoldAffectedModules =
                        allModulesWithAst
                            |> List.filter
                                (\file ->
                                    staleFileContents
                                        |> List.any (\stale -> stale.path == file.path)
                                )

                    importersFoldSupportingPaths =
                        let
                            stalePathSet =
                                staleFileContents
                                    |> List.map .path
                                    |> Set.fromList

                            requiredPaths =
                                importersFoldMissedPaths
                                    |> List.foldl
                                        (\path acc ->
                                            Set.insert path
                                                (Set.union acc (DepGraph.directReverseDeps depGraph path))
                                        )
                                        Set.empty
                        in
                        requiredPaths
                            |> Set.diff stalePathSet
                            |> Set.toList

                    importersAffectedModules =
                        let
                            affectedEvalPaths =
                                importersMissedPaths
                                    |> List.foldl
                                            (\path acc ->
                                                Set.union acc (DepGraph.directReverseDeps depGraph path)
                                            )
                                            Set.empty
                        in
                        allModulesWithAst
                            |> List.filter (\file -> Set.member file.path affectedEvalPaths)

                    useSplitImportersCache =
                        case config.importersCacheMode of
                            ImportersAuto ->
                                if List.length staleFileContents < List.length allFileContents && List.length importersAffectedModules > 1 then
                                    True

                                else
                                    False

                            ImportersSplit ->
                                True

                            ImportersFresh ->
                                False

                    useSplitImportersFoldCache =
                        case config.importersCacheMode of
                            ImportersAuto ->
                                List.length staleFileContents < List.length allFileContents
                                    && (importersFoldMissedPaths
                                            |> List.any
                                                (\path ->
                                                    staleFileContents
                                                        |> List.any (\file -> file.path == path)
                                                        |> not
                                                )
                                       )

                            ImportersSplit ->
                                True

                            ImportersFresh ->
                                False

                    depsAffectedModules =
                        let
                            affectedEvalPaths =
                                depsMissedPaths
                                    |> List.foldl
                                        (\path acc ->
                                            Set.union acc (DepGraph.transitiveDeps depGraph path)
                                        )
                                        Set.empty
                        in
                        allModulesWithAst
                            |> List.filter (\file -> Set.member file.path affectedEvalPaths)

                    useSplitDepsCache =
                        case config.depsCacheMode of
                            ImportersAuto ->
                                if List.length staleFileContents < List.length allFileContents && List.length depsAffectedModules > 1 then
                                    True

                                else
                                    False

                            ImportersSplit ->
                                True

                            ImportersFresh ->
                                False

                    sliceCounters =
                        Dict.fromList
                            [ ( "project.importers.rule_count", List.length importersAllRules )
                            , ( "project.importers_fold.rule_count", List.length importersFoldOnlyRules )
                            , ( "project.importers_fold.cache_hits", importersFoldChecks |> List.filter .hit |> List.length )
                            , ( "project.importers_fold.cache_misses", List.length importersFoldMissedPaths )
                            , ( "project.importers_fold.affected_modules", List.length importersFoldAffectedModules )
                            , ( "project.importers.cache_hits", importersChecks |> List.filter .hit |> List.length )
                            , ( "project.importers.cache_misses", List.length importersMissedPaths )
                            , ( "project.importers.affected_modules", List.length importersAffectedModules )
                            , ( "project.deps.rule_count", List.length dependenciesOfRules )
                            , ( "project.deps.cache_hits", depsChecks |> List.filter .hit |> List.length )
                            , ( "project.deps.cache_misses", List.length depsMissedPaths )
                            , ( "project.deps.affected_modules", List.length depsAffectedModules )
                            , ( "project.full.rule_count", List.length fullProjectRules )
                            , ( "project.importers_fold.cache_check_ms", importersFoldChecksTimed.stage.ms )
                            , ( "project.importers.cache_check_ms", importersChecksTimed.stage.ms )
                            , ( "project.deps.cache_check_ms", depsChecksTimed.stage.ms )
                            ]
                in
                Do.do
                    (withTiming "project.importers_fold.cached_output_read"
                        (readPerFileRuleOutputs
                            prImportersFoldCacheDir
                            (if importersFoldAllHit then
                                importersFoldPerFileKeys |> List.map .path

                             else
                                importersFoldChecks
                                    |> List.filter .hit
                                    |> List.map .path
                            )
                        )
                    )
                <| \importersFoldCachedOutputsTimed ->
                Do.do
                    (withTiming "project.importers.cached_output_read"
                        (readPerFileRuleOutputs
                            prCacheDir
                            (if importersAllHit then
                                importersPerFileKeys |> List.map .path

                             else
                                importersChecks
                                    |> List.filter .hit
                                    |> List.map .path
                            )
                        )
                    )
                <| \importersCachedOutputsTimed ->
                Do.do
                    (withTiming "project.deps.cached_output_read"
                        (readPerFileRuleOutputs
                            doCacheDir
                            (if depsAllHit then
                                depsPerFileKeys |> List.map .path

                             else
                                depsChecks
                                    |> List.filter .hit
                                    |> List.map .path
                            )
                        )
                    )
                <| \depsCachedOutputsTimed ->
                let
                    importersFoldCachedOutputs =
                        importersFoldCachedOutputsTimed.value

                    importersCachedOutputs =
                        importersCachedOutputsTimed.value

                    depsCachedOutputs =
                        depsCachedOutputsTimed.value
                in
                Do.do (BackendTask.succeed MemoRuntime.emptyMemoCache) <| \initialMemoCache ->
                Do.do
                    (withTiming "project.importers_fold.eval_total"
                        (if importersFoldAllHit then
                            BackendTask.succeed
                                { output = ""
                                , memoCache = initialMemoCache
                                , memoStats = MemoRuntime.emptyMemoStats
                                , loadedRuleCaches = emptyLoadedRuleCaches
                                , counters = Dict.empty
                                }

                         else
                            if useSplitImportersFoldCache then
                            runProjectRulesWithWarmRuleCaches
                                "project.importers_fold"
                                (Path.toString config.buildDirectory)
                                reviewProject
                                importersFoldIndices
                                importersFoldSupportingPaths
                                importersFoldAffectedModules
                                (staleFileContents |> List.map .path)
                                config.memoizedFunctions
                                config.memoProfile
                                initialMemoCache

                            else
                                Do.do
                                    (withTiming "project.importers_fold.fresh_eval"
                                    (runProjectRulesWithCacheWrite
                                        (Path.toString config.buildDirectory)
                                        reviewProject
                                        importersFoldIndices
                                        importersFoldAffectedModules
                                        (staleFileContents |> List.map .path)
                                        config.memoizedFunctions
                                        config.memoProfile
                                        initialMemoCache
                                        )
                                    )
                                <| \freshTimed ->
                                BackendTask.succeed
                                    { output = freshTimed.value.output
                                    , memoCache = freshTimed.value.memoCache
                                    , memoStats = freshTimed.value.memoStats
                                    , loadedRuleCaches = emptyLoadedRuleCaches
                                    , counters =
                                        Dict.fromList
                                            [ ( "project.importers_fold.mode.fresh", 1 )
                                            , ( "project.importers_fold.mode.split", 0 )
                                            , ( "project.importers_fold.fresh_eval_ms", freshTimed.stage.ms )
                                            ]
                                    }
                        )
                    )
                <| \importersFoldEvalTimed ->
                let
                    importersFoldEvalBase =
                        importersFoldEvalTimed.value

                    importersFoldEval =
                        { importersFoldEvalBase
                            | counters =
                                Dict.insert "project.importers_fold.eval_total_ms"
                                    importersFoldEvalTimed.stage.ms
                                    importersFoldEvalBase.counters
                        }
                in
                Do.do
                    (withTiming "project.importers.eval_total"
                        (if importersAllHit then
                            BackendTask.succeed
                                { output = ""
                                , memoCache = importersFoldEval.memoCache
                                , memoStats = MemoRuntime.emptyMemoStats
                                , loadedRuleCaches = emptyLoadedRuleCaches
                                , counters = Dict.empty
                                }

                         else
                            if useSplitImportersCache then
                            runProjectRulesWithWarmRuleCaches
                                "project.importers"
                                (Path.toString config.buildDirectory)
                                reviewProject
                                ioIndices
                                (importersAffectedModules
                                        |> List.map .path
                                        |> List.filter
                                            (\path ->
                                                staleFileContents
                                                    |> List.any (\file -> file.path == path)
                                                    |> not
                                            )
                                    )
                                    importersAffectedModules
                                    importersMissedPaths
                                    config.memoizedFunctions
                                    config.memoProfile
                                    importersFoldEval.memoCache

                            else
                                Do.do
                                    (withTiming "project.importers.fresh_eval"
                                    (runProjectRulesWithCacheWrite
                                        (Path.toString config.buildDirectory)
                                        reviewProject
                                        ioIndices
                                        importersAffectedModules
                                            importersMissedPaths
                                            config.memoizedFunctions
                                            config.memoProfile
                                            importersFoldEval.memoCache
                                        )
                                    )
                                <| \freshTimed ->
                                BackendTask.succeed
                                    { output = freshTimed.value.output
                                    , memoCache = freshTimed.value.memoCache
                                    , memoStats = freshTimed.value.memoStats
                                    , loadedRuleCaches = emptyLoadedRuleCaches
                                    , counters =
                                        Dict.fromList
                                            [ ( "project.importers.mode.fresh", 1 )
                                            , ( "project.importers.mode.split", 0 )
                                            , ( "project.importers.fresh_eval_ms", freshTimed.stage.ms )
                                            ]
                                    }
                        )
                    )
                <| \importersEvalTimed ->
                let
                    importersEvalBase =
                        importersEvalTimed.value

                    importersEval =
                        { importersEvalBase
                            | counters =
                                Dict.insert "project.importers.eval_total_ms"
                                    importersEvalTimed.stage.ms
                                    importersEvalBase.counters
                        }
                in
                Do.do
                    (withTiming "project.deps.eval_total"
                        (if depsAllHit then
                            BackendTask.succeed
                                { output = ""
                                , memoCache = importersEval.memoCache
                                , memoStats = MemoRuntime.emptyMemoStats
                                , loadedRuleCaches = emptyLoadedRuleCaches
                                , counters = Dict.empty
                                }

                         else
                            if useSplitDepsCache then
                            runProjectRulesWithWarmRuleCaches
                                "project.deps"
                                (Path.toString config.buildDirectory)
                                reviewProject
                                doIndices
                                (depsAffectedModules
                                        |> List.map .path
                                        |> List.filter
                                            (\path ->
                                                staleFileContents
                                                    |> List.any (\file -> file.path == path)
                                                    |> not
                                            )
                                    )
                                    depsAffectedModules
                                    depsMissedPaths
                                    config.memoizedFunctions
                                    config.memoProfile
                                    importersEval.memoCache
                                    |> BackendTask.map
                                        (\depsResult ->
                                            { depsResult
                                                | counters =
                                                    mergeCounterDicts
                                                        [ depsResult.counters
                                                        , Dict.fromList
                                                            [ ( "project.deps.mode.fresh", 0 )
                                                            , ( "project.deps.mode.split", 1 )
                                                            , ( "project.deps.rule_cache.entries", depsResult.loadedRuleCaches.entryCount )
                                                            , ( "project.deps.rule_cache.loaded_bytes", depsResult.loadedRuleCaches.bytes )
                                                            ]
                                                        ]
                                            }
                                        )

                            else
                                Do.do
                                    (withTiming "project.deps.fresh_eval"
                                    (runProjectRulesWithCacheWrite
                                        (Path.toString config.buildDirectory)
                                        reviewProject
                                        doIndices
                                        depsAffectedModules
                                            depsMissedPaths
                                            config.memoizedFunctions
                                            config.memoProfile
                                            importersEval.memoCache
                                        )
                                    )
                                <| \depsTimed ->
                                BackendTask.succeed
                                    { output = depsTimed.value.output
                                    , memoCache = depsTimed.value.memoCache
                                    , memoStats = depsTimed.value.memoStats
                                    , loadedRuleCaches = emptyLoadedRuleCaches
                                    , counters =
                                        Dict.fromList
                                            [ ( "project.deps.mode.fresh", 1 )
                                            , ( "project.deps.mode.split", 0 )
                                            , ( "project.deps.fresh_eval_ms", depsTimed.stage.ms )
                                            ]
                                    }
                        )
                    )
                <| \depsEvalTimed ->
                let
                    depsEvalBase =
                        depsEvalTimed.value

                    depsEval =
                        { depsEvalBase
                            | counters =
                                Dict.insert "project.deps.eval_total_ms"
                                    depsEvalTimed.stage.ms
                                    depsEvalBase.counters
                        }
                in
                Do.do
                    (withTiming "project.postprocess"
                        (deferTask
                            (\() ->
                                let
                                    ioFoldErrors =
                                        if importersFoldAllHit then
                                            []

                                        else
                                            parseReviewOutput importersFoldEval.output

                                    ioErrors =
                                        if importersAllHit then
                                            []

                                        else
                                            parseReviewOutput importersEval.output

                                    doErrors =
                                        if depsAllHit then
                                            []

                                        else
                                            parseReviewOutput depsEval.output

                                    freshImportersFoldOutputs =
                                        importersFoldMissedPaths
                                            |> List.map
                                                (\path ->
                                                    ioFoldErrors
                                                        |> List.filter (\err -> err.filePath == path)
                                                        |> List.map formatErrorLine
                                                        |> String.join "\n"
                                                )

                                    freshImportersOutputs =
                                        importersMissedPaths
                                            |> List.map
                                                (\path ->
                                                    ioErrors
                                                        |> List.filter (\err -> err.filePath == path)
                                                        |> List.map formatErrorLine
                                                        |> String.join "\n"
                                                )

                                    freshDepsOutputs =
                                        depsMissedPaths
                                            |> List.map
                                                (\path ->
                                                    doErrors
                                                        |> List.filter (\err -> err.filePath == path)
                                                        |> List.map formatErrorLine
                                                        |> String.join "\n"
                                                )

                                    partialMemoCounters =
                                        mergeMemoStats
                                            (mergeMemoStats importersFoldEval.memoStats importersEval.memoStats)
                                            depsEval.memoStats
                                            |> memoStatsCounters "memo.partial_project"

                                    importersRuleCacheCounters =
                                        mergeCounterDicts
                                            [ Dict.fromList
                                                [ ( "project.importers_fold.rule_cache.entries", importersFoldEval.loadedRuleCaches.entryCount )
                                                , ( "project.importers_fold.rule_cache.loaded_bytes", importersFoldEval.loadedRuleCaches.bytes )
                                                , ( "project.importers_fold.cached_output_read_ms", importersFoldCachedOutputsTimed.stage.ms )
                                                , ( "project.importers.rule_cache.entries", importersEval.loadedRuleCaches.entryCount )
                                                , ( "project.importers.rule_cache.loaded_bytes", importersEval.loadedRuleCaches.bytes )
                                                , ( "project.importers.cached_output_read_ms", importersCachedOutputsTimed.stage.ms )
                                                , ( "project.deps.cached_output_read_ms", depsCachedOutputsTimed.stage.ms )
                                                , ( "project.importers.eval_total_ms"
                                                  , Maybe.withDefault 0 (Dict.get "project.importers.eval_total_ms" importersEval.counters)
                                                        + Maybe.withDefault 0 (Dict.get "project.importers_fold.eval_total_ms" importersFoldEval.counters)
                                                  )
                                                ]
                                            , importersFoldEval.counters
                                            , importersEval.counters
                                            , depsEval.counters
                                            ]
                                in
                                BackendTask.succeed
                                    { ioFoldErrors = ioFoldErrors
                                    , ioErrors = ioErrors
                                    , doErrors = doErrors
                                    , freshImportersFoldOutputs = freshImportersFoldOutputs
                                    , freshImportersOutputs = freshImportersOutputs
                                    , freshDepsOutputs = freshDepsOutputs
                                    , partialMemoCounters = partialMemoCounters
                                    , importersRuleCacheCounters = importersRuleCacheCounters
                                    }
                            )
                        )
                    )
                <| \postprocessTimed ->
                let
                    ioFoldErrors =
                        postprocessTimed.value.ioFoldErrors

                    ioErrors =
                        postprocessTimed.value.ioErrors

                    doErrors =
                        postprocessTimed.value.doErrors

                    freshImportersFoldOutputs =
                        postprocessTimed.value.freshImportersFoldOutputs

                    freshImportersOutputs =
                        postprocessTimed.value.freshImportersOutputs

                    freshDepsOutputs =
                        postprocessTimed.value.freshDepsOutputs

                    partialMemoCounters =
                        postprocessTimed.value.partialMemoCounters

                    importersRuleCacheCounters =
                        Dict.insert "project.postprocess_ms"
                            postprocessTimed.stage.ms
                            postprocessTimed.value.importersRuleCacheCounters
                in
                Do.do
                    (appendMemoProfileLog
                        config.memoProfile
                        (Path.toString config.buildDirectory)
                        "partial-project-slices"
                        (MemoRuntime.entryCount depsEval.memoCache)
                        (mergeMemoStats
                            (mergeMemoStats importersFoldEval.memoStats importersEval.memoStats)
                            depsEval.memoStats
                        )
                    )
                <| \_ ->
                Do.do
                    (withTiming "project.importers_fold.cache_write"
                        (if importersFoldAllHit then
                            BackendTask.succeed ()

                         else
                            writePerFileRuleOutputs prImportersFoldCacheDir importersFoldPerFileKeys importersFoldMissedPaths ioFoldErrors
                        )
                    )
                <| \importersFoldWriteTimed ->
                Do.do
                    (withTiming "project.importers.cache_write"
                        (if importersAllHit then
                            BackendTask.succeed ()

                         else
                            writePerFileRuleOutputs prCacheDir importersPerFileKeys importersMissedPaths ioErrors
                        )
                    )
                <| \importersWriteTimed ->
                Do.do
                    (withTiming "project.deps.cache_write"
                        (if depsAllHit then
                            BackendTask.succeed ()

                         else
                            writePerFileRuleOutputs doCacheDir depsPerFileKeys depsMissedPaths doErrors
                        )
                    )
                <| \depsWriteTimed ->
                let
                    importersResult =
                        importersFoldCachedOutputs
                            ++ freshImportersFoldOutputs
                            ++ importersCachedOutputs
                            ++ freshImportersOutputs
                            |> List.filter (\s -> not (String.isEmpty (String.trim s)))
                            |> String.join "\n"

                    depsOfResult =
                        depsCachedOutputs
                            ++ freshDepsOutputs
                            |> List.filter (\s -> not (String.isEmpty (String.trim s)))
                            |> String.join "\n"

                    fpIndices =
                        fullProjectRules |> List.map .index

                    fullProjectProfile =
                        mergeProfiles FullProject fullProjectRules

                    fpCacheKey =
                        "pr-fp|" ++ helperHash ++ "|" ++ (fpIndices |> List.map String.fromInt |> String.join ",") ++ "|"
                            ++ (allFileAspectHashes
                                    |> Dict.toList
                                    |> List.sortBy Tuple.first
                                    |> List.map (\( _, h ) -> narrowAspects fullProjectProfile h)
                                    |> String.join "|"
                                    |> FNV1a.hash
                                    |> String.fromInt
                               )

                    fpKeyPath =
                        Path.toString config.buildDirectory ++ "/pr-fp-cache.key"

                    fpDataPath =
                        Path.toString config.buildDirectory ++ "/pr-fp-cache.txt"

                    combinedProjectOutputs =
                        [ moduleRuleOutput, importersResult, depsOfResult ]
                            |> List.filter (not << String.isEmpty)
                            |> String.join "\n"
                in
                if List.isEmpty fullProjectRules then
                    BackendTask.succeed
                        { output = combinedProjectOutputs
                        , counters =
                            mergeCounterDicts
                                [ sliceCounters
                                , importersRuleCacheCounters
                                , partialMemoCounters
                                , Dict.fromList
                                    [ ( "project.importers_fold.cache_write_ms", importersFoldWriteTimed.stage.ms )
                                    , ( "project.importers.cache_write_ms", importersWriteTimed.stage.ms )
                                    , ( "project.deps.cache_write_ms", depsWriteTimed.stage.ms )
                                    ]
                                ]
                        }

                else
                    Do.do
                        (evalProjectRulesWithWarmState
                            config
                            reviewProject
                            helperHash
                            fpIndices
                            allFileContents
                            staleFileContents
                            (Just
                                { keyPath = fpKeyPath
                                , dataPath = fpDataPath
                                , key = fpCacheKey
                                }
                            )
                            depsEval.memoCache
                        )
                    <| \fpResult ->
                    BackendTask.succeed
                        { output =
                            ([ combinedProjectOutputs, fpResult.output ]
                                |> List.filter (\s -> not (String.isEmpty (String.trim s)) && not (String.startsWith "ERROR:" s))
                                |> String.join "\n"
                            )
                        , counters =
                            mergeCounterDicts
                                [ sliceCounters
                                , importersRuleCacheCounters
                                , partialMemoCounters
                                , Dict.fromList
                                    [ ( "project.importers_fold.cache_write_ms", importersFoldWriteTimed.stage.ms )
                                    , ( "project.importers.cache_write_ms", importersWriteTimed.stage.ms )
                                    , ( "project.deps.cache_write_ms", depsWriteTimed.stage.ms )
                                    ]
                                , memoStatsCounters "memo.full_project" fpResult.memoStats
                                , Dict.fromList
                                    [ ( "project.full.used_cached_project", boolToInt fpResult.usedCachedProject )
                                    ]
                                ]
                        }
            )
        )
    <| \projectPhaseTimed ->
    BackendTask.succeed
        { output = projectPhaseTimed.value.output
        , perf =
            { moduleRuleEvalMs = modulePhaseTimed.stage.ms
            , projectRuleEvalMs = projectPhaseTimed.stage.ms
            , counters =
                mergeCounterDicts
                    [ basePerfCounters
                    , Dict.fromList
                        [ ( "module_rules.stale_modules", List.length staleModulesWithAst )
                        ]
                    , projectPhaseTimed.value.counters
                    ]
            }
        }


{-| Full hybrid evaluation (ColdMiss path). Uses Cache.compute for all rules.
-}
loadAndEvalHybrid :
    Config
    -> List { index : Int, name : String, ruleType : RuleType }
    -> List { path : String, source : String }
    -> BackendTask FatalError String
loadAndEvalHybrid config ruleInfo targetFileContents =
    Do.do (loadReviewProject config) <| \reviewProject ->
    loadAndEvalHybridWithProject config reviewProject ruleInfo targetFileContents


loadAndEvalHybridWithProject :
    Config
    -> InterpreterProject
    -> List { index : Int, name : String, ruleType : RuleType }
    -> List { path : String, source : String }
    -> BackendTask FatalError String
loadAndEvalHybridWithProject config reviewProject ruleInfo targetFileContents =
    let
        helperHash =
            FNV1a.hash reviewRunnerHelperSource |> String.fromInt

        projectSemanticKey =
            computeSemanticKey targetFileContents

        modulesWithAst =
            targetFileContents
                |> List.filterMap
                    (\{ path, source } ->
                        encodeFileAsJson source
                            |> Maybe.map (\astJson -> { path = path, source = source, astJson = astJson })
                    )

        -- Compute per-file aspect hashes for narrow cache keys
        allFileAspectHashes : Dict String SemanticHash.FileAspectHashes
        allFileAspectHashes =
            targetFileContents
                |> List.map
                    (\{ path, source } ->
                        ( path, SemanticHash.computeAspectHashesFromSource source )
                    )
                |> Dict.fromList

        depGraph : DepGraph.Graph
        depGraph =
            DepGraph.buildGraph
                { sourceDirectories = []
                , files =
                    targetFileContents
                        |> List.map (\{ path, source } -> { filePath = path, content = source })
                }

        moduleRules =
            ruleInfo |> List.filter (\r -> r.ruleType == ModuleRule)

        projectRules =
            ruleInfo |> List.filter (\r -> r.ruleType == ProjectRule)

        moduleRuleIndices =
            moduleRules |> List.map .index

        moduleRuleProfile =
            mergeProfiles NoCrossModule moduleRules

        -- Module rules: batch all module rules for each file
        moduleRuleMonads =
            if List.isEmpty moduleRules then
                []

            else
                modulesWithAst
                    |> List.indexedMap
                        (\fileIdx file ->
                            let
                                fileHashes =
                                    Dict.get file.path allFileAspectHashes
                                        |> Maybe.withDefault (SemanticHash.computeAspectHashesFromSource file.source)

                                fileNarrowKey =
                                    narrowCacheKey moduleRuleProfile file.path fileHashes allFileAspectHashes depGraph

                                cacheKey =
                                    "mrf|" ++ helperHash ++ "|" ++ (moduleRuleIndices |> List.map String.fromInt |> String.join ",") ++ "|" ++ file.path ++ "|" ++ fileNarrowKey
                            in
                            Cache.do (Cache.writeFile cacheKey Cache.succeed) <| \keyHash ->
                            Cache.compute [ "mrf", file.path ]
                                keyHash
                                (\() ->
                                    case
                                        InterpreterProject.prepareAndEval reviewProject
                                            { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                                            , expression = buildExpressionForRules moduleRuleIndices [ file ]
                                            , sourceOverrides = [ reviewRunnerHelperSource ]
                                            }
                                    of
                                        Ok s ->
                                            s

                                        Err e ->
                                            e
                                )
                            <| \hash ->
                            Cache.succeed
                                { filename = Path.path ("mrf-" ++ String.fromInt fileIdx)
                                , hash = hash
                                }
                        )

        allMonads =
            moduleRuleMonads
                |> Cache.sequence
                |> Cache.andThen Cache.combine
    in
    -- Step 1: Run module rules via Cache.compute (per-file, disk-cached)
    Cache.run { jobs = Nothing } config.buildDirectory allMonads
        |> BackendTask.andThen
            (\cacheResult ->
                let
                    mrFiles =
                        if List.isEmpty moduleRules then
                            []

                        else
                            modulesWithAst
                                |> List.indexedMap
                                    (\fileIdx _ ->
                                        File.rawFile
                                            (Path.toString cacheResult.output ++ "/mrf-" ++ String.fromInt fileIdx)
                                            |> BackendTask.allowFatal
                                    )
                in
                mrFiles
                    |> BackendTask.Extra.combine
                    |> BackendTask.map
                        (\outputs ->
                            outputs
                                |> List.filter
                                    (\s ->
                                        not (String.isEmpty (String.trim s))
                                            && not (String.startsWith "ERROR:" s)
                                    )
                                |> String.join "\n"
                        )
            )
        |> BackendTask.andThen
            (\moduleRuleOutput ->
                -- Step 2: Project rules — two-pass with Value injection
                if List.isEmpty projectRules then
                    BackendTask.succeed moduleRuleOutput

                else
                    let
                        prIndices =
                            projectRules |> List.map .index

                        -- Use FullProject profile for combined project rules (conservative)
                        -- Individual rules could be narrower, but they run together
                        prNarrowKey =
                            let
                                combinedProfile =
                                    { expressionDep = True
                                    , declarationNameDep = True
                                    , importDep = True
                                    , exposingDep = True
                                    , customTypeDep = True
                                    , crossModuleDep = FullProject
                                    }

                                dummyHashes =
                                    { expressionsHash = projectSemanticKey
                                    , declNamesHash = projectSemanticKey
                                    , importsHash = projectSemanticKey
                                    , exposingHash = projectSemanticKey
                                    , customTypesHash = projectSemanticKey
                                    , fullHash = projectSemanticKey
                                    }
                            in
                            narrowCacheKey combinedProfile "" dummyHashes allFileAspectHashes depGraph

                        prCacheKey =
                            "pr-all|" ++ helperHash ++ "|" ++ (prIndices |> List.map String.fromInt |> String.join ",") ++ "|" ++ prNarrowKey

                        prCacheKeyPath =
                            Path.toString config.buildDirectory ++ "/pr-cache.key"

                        prCacheDataPath =
                            Path.toString config.buildDirectory ++ "/pr-cache.txt"
                    in
                    -- Check project rule disk cache
                    File.rawFile prCacheKeyPath
                        |> BackendTask.toResult
                        |> BackendTask.andThen
                            (\keyResult ->
                                case keyResult of
                                    Ok storedKey ->
                                        if storedKey == prCacheKey then
                                            File.rawFile prCacheDataPath
                                                |> BackendTask.toResult
                                                |> BackendTask.andThen
                                                    (\dataResult ->
                                                        case dataResult of
                                                            Ok cachedPrErrors ->
                                                                BackendTask.succeed cachedPrErrors

                                                            Err _ ->
                                                                evalProjectRulesSingle (Path.toString config.buildDirectory) config.memoizedFunctions config.memoProfile reviewProject prIndices modulesWithAst prCacheKeyPath prCacheDataPath prCacheKey
                                                    )

                                        else
                                            evalProjectRulesSingle (Path.toString config.buildDirectory) config.memoizedFunctions config.memoProfile reviewProject prIndices modulesWithAst prCacheKeyPath prCacheDataPath prCacheKey

                                    Err _ ->
                                        evalProjectRulesSingle (Path.toString config.buildDirectory) config.memoizedFunctions config.memoProfile reviewProject prIndices modulesWithAst prCacheKeyPath prCacheDataPath prCacheKey
                            )
                        |> BackendTask.map
                            (\prOutput ->
                                let
                                    combined =
                                        [ moduleRuleOutput, prOutput ]
                                            |> List.filter (\s -> not (String.isEmpty (String.trim s)) && not (String.startsWith "ERROR:" s))
                                            |> String.join "\n"
                                in
                                combined
                            )
            )


{--  OLD BODY
        perFileKeys =
            targetFileContents
                |> List.map
                    (\file ->
                        ( file.path
                        , computeSemanticKey [ file ]
                        )
                    )
                |> Dict.fromList

        -- Combined per-file key for module rules: only includes individual file hashes
        -- so changing file A doesn't invalidate file B's cache
        moduleRuleKey : String
        moduleRuleKey =
            perFileKeys
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.map (\( path, key ) -> path ++ ":" ++ key)
                |> String.join ","
                |> FNV1a.hash
                |> String.fromInt

        -- Compute cache key per rule.
        -- Module rules use per-file composite key — only changes when the specific
        -- set of per-file hashes changes. Since module rules are independent per file,
        -- a change to one file only affects that file's contribution.
        -- Project rules use the global project key — any change invalidates.
        ruleCacheKey : { index : Int, name : String, ruleType : RuleType } -> String
        ruleCacheKey rule =
            case rule.ruleType of
                ModuleRule ->
                    -- For module rules, we use a composite of per-file hashes.
                    -- This is the SAME as projectSemanticKey for now (all files),
                    -- but on Phase 3 miss we can re-eval only changed-file module rules.
                    "mr|" ++ String.fromInt rule.index ++ "|" ++ helperHash ++ "|" ++ projectSemanticKey

                ProjectRule ->
                    "pr|" ++ String.fromInt rule.index ++ "|" ++ helperHash ++ "|" ++ projectSemanticKey
    in
    -- Phase 1: Check all per-rule caches
    Do.do
        (ruleInfo
            |> List.map
                (\rule ->
                    let
                        keyPath =
                            ruleCacheDir ++ "/" ++ String.fromInt rule.index ++ ".key"

                        dataPath =
                            ruleCacheDir ++ "/" ++ String.fromInt rule.index ++ ".txt"

                        expectedKey =
                            ruleCacheKey rule
                    in
                    File.rawFile keyPath
                        |> BackendTask.toResult
                        |> BackendTask.andThen
                            (\keyResult ->
                                case keyResult of
                                    Ok storedKey ->
                                        if storedKey == expectedKey then
                                            File.rawFile dataPath
                                                |> BackendTask.toResult
                                                |> BackendTask.map
                                                    (\dataResult ->
                                                        case dataResult of
                                                            Ok cached ->
                                                                { index = rule.index, cached = Just cached }

                                                            Err _ ->
                                                                { index = rule.index, cached = Nothing }
                                                    )

                                        else
                                            BackendTask.succeed { index = rule.index, cached = Nothing }

                                    Err _ ->
                                        BackendTask.succeed { index = rule.index, cached = Nothing }
                            )
                )
            |> BackendTask.Extra.combine
        )
    <| \cacheResults ->
    let
        allHit =
            List.all (\r -> r.cached /= Nothing) cacheResults

        cachedOutput =
            cacheResults
                |> List.filterMap .cached
                |> List.filter (\s -> not (String.isEmpty (String.trim s)) && not (String.startsWith "ERROR:" s))
                |> String.join "\n"
    in
    if allHit then
        -- Phase 2: All rules cached — return combined results
        BackendTask.succeed cachedOutput

    else
        -- Phase 3: Some rules missed — run monolithic eval for ALL rules
        Do.do (loadAndEval config targetFileContents) <| \{ output } ->
        -- Split errors per rule and cache to disk
        let
            errors =
                parseReviewOutput output

            errorsByRule =
                ruleInfo
                    |> List.map
                        (\rule ->
                            { index = rule.index
                            , errors =
                                errors
                                    |> List.filter (\e -> e.ruleName == rule.name)
                                    |> List.map formatErrorLine
                                    |> String.join "\n"
                            }
                        )
        in
        Do.do (Script.exec "mkdir" [ "-p", ruleCacheDir ]) <| \_ ->
        Do.do
            (errorsByRule
                |> List.map
                    (\ruleResult ->
                        let
                            rule =
                                List.drop ruleResult.index ruleInfo |> List.head

                            keyPath =
                                ruleCacheDir ++ "/" ++ String.fromInt ruleResult.index ++ ".key"

                            dataPath =
                                ruleCacheDir ++ "/" ++ String.fromInt ruleResult.index ++ ".txt"

                            key =
                                case rule of
                                    Just r ->
                                        ruleCacheKey r

                                    Nothing ->
                                        ""
                        in
                        Script.writeFile { path = keyPath, body = key }
                            |> BackendTask.allowFatal
                            |> BackendTask.andThen
                                (\_ ->
                                    Script.writeFile { path = dataPath, body = ruleResult.errors }
                                        |> BackendTask.allowFatal
                                )
                    )
                |> BackendTask.Extra.combine
            )
        <| \_ ->
OLD BODY END --}


{-| Build elm-review cache marker intercepts.

Pre-loads any previously cached rule Values from disk, then builds
intercepts for the 3 marker functions:
- initialCacheMarker: returns loaded cache if available, else default
- finalCacheMarker: identity (cache extracted from result after eval)
- createContextHashMarker: deep hash of context Value
-}
boolStr : Bool -> String
boolStr b =
    if b then
        "True"

    else
        "False"


cacheFileComponent : String -> String
cacheFileComponent path =
    path
        |> String.replace "/" "_"


checkPerFileRuleCache : String -> List PerFileRuleKey -> BackendTask FatalError (List PerFileRuleCheck)
checkPerFileRuleCache cacheDir perFileKeys =
    perFileKeys
        |> List.map
            (\{ path, narrowKey } ->
                File.rawFile (cacheDir ++ "/" ++ cacheFileComponent path ++ ".key")
                    |> BackendTask.toResult
                    |> BackendTask.map
                        (\result ->
                            case result of
                                Ok storedKey ->
                                    { path = path, hit = storedKey == narrowKey }

                                Err _ ->
                                    { path = path, hit = False }
                        )
            )
        |> BackendTask.Extra.combine


readPerFileRuleOutputs : String -> List String -> BackendTask FatalError (List String)
readPerFileRuleOutputs cacheDir paths =
    paths
        |> List.map
            (\path ->
                File.rawFile (cacheDir ++ "/" ++ cacheFileComponent path ++ ".txt")
                    |> BackendTask.allowFatal
            )
        |> BackendTask.Extra.combine


writePerFileRuleOutputs :
    String
    -> List PerFileRuleKey
    -> List String
    -> List ReviewError
    -> BackendTask FatalError ()
writePerFileRuleOutputs cacheDir perFileKeys targetPaths errors =
    perFileKeys
        |> List.filter (\{ path } -> List.member path targetPaths)
        |> List.map
            (\{ path, narrowKey } ->
                let
                    safePath =
                        cacheFileComponent path

                    fileErrors =
                        errors
                            |> List.filter (\err -> err.filePath == path)
                            |> List.map formatErrorLine
                            |> String.join "\n"
                in
                Script.writeFile { path = cacheDir ++ "/" ++ safePath ++ ".key", body = narrowKey }
                    |> BackendTask.allowFatal
                    |> BackendTask.andThen
                        (\_ ->
                            Script.writeFile { path = cacheDir ++ "/" ++ safePath ++ ".txt", body = fileErrors }
                                |> BackendTask.allowFatal
                        )
            )
        |> BackendTask.Extra.combine
        |> BackendTask.map (\_ -> ())


buildReviewIntercepts :
    List String
    -> SharedModulePayloads
    -> LoadedRuleCaches
    -> FastDict.Dict String Types.Intercept
buildReviewIntercepts finalCacheAllowedPaths sharedModulePayloads loadedRuleCaches =
    FastDict.fromList
        [ ( "Review.Rule.initialCachePartsMarker"
          , Types.Intercept
                (\_ args _ _ ->
                    case args of
                        [ Types.String ruleName, Types.Int ruleId, defaultSplitCache ] ->
                            case defaultSplitCache of
                                Types.Record fields ->
                                    let
                                        ruleKey =
                                            ruleCacheValueKey ruleName ruleId

                                        baseCache =
                                            Dict.get ruleKey loadedRuleCaches.baseCaches
                                                |> Maybe.withDefault
                                                    (FastDict.get "baseCache" fields
                                                        |> Maybe.withDefault Types.Unit
                                                    )

                                        moduleContexts =
                                            Dict.get ruleKey loadedRuleCaches.moduleEntries
                                                |> Maybe.withDefault Dict.empty
                                                |> Dict.toList
                                                |> List.map
                                                    (\( modulePath, value ) ->
                                                        Types.Tuple (Types.String modulePath) value
                                                    )
                                    in
                                    Types.EvOk
                                        (Types.Record
                                            (FastDict.insert "baseCache" baseCache
                                                (FastDict.insert "moduleContexts" (Types.List moduleContexts) fields)
                                            )
                                        )

                                _ ->
                                    Types.EvOk defaultSplitCache

                        _ ->
                            Types.EvOk (args |> List.reverse |> List.head |> Maybe.withDefault Types.Unit)
                )
          )
        , ( "Review.Rule.initialCacheAllowedPathsMarker"
          , Types.Intercept
                (\_ _ _ _ ->
                    Types.EvOk (Types.List [])
                )
          )
        , ( "Review.Rule.finalCacheAllowedPathsMarker"
          , Types.Intercept
                (\_ _ _ _ ->
                    Types.EvOk (Types.List (List.map Types.String finalCacheAllowedPaths))
                )
          )
        , ( "ReviewRunnerHelper.moduleHandlePathMarker"
          , Types.Intercept
                (\_ args _ _ ->
                    case args of
                        [ Types.Int handle ] ->
                            sharedModulePayloads.paths
                                |> Array.get handle
                                |> Maybe.map Types.String
                                |> Maybe.withDefault (Types.String "")
                                |> Types.EvOk

                        _ ->
                            Types.EvOk (Types.String "")
                )
          )
        , ( "ReviewRunnerHelper.moduleHandleSourceMarker"
          , Types.Intercept
                (\_ args _ _ ->
                    case args of
                        [ Types.Int handle ] ->
                            sharedModulePayloads.sources
                                |> Array.get handle
                                |> Maybe.map Types.String
                                |> Maybe.withDefault (Types.String "")
                                |> Types.EvOk

                        _ ->
                            Types.EvOk (Types.String "")
                )
          )
        , ( "ReviewRunnerHelper.moduleHandleAstJsonMarker"
          , Types.Intercept
                (\_ args _ _ ->
                    case args of
                        [ Types.Int handle ] ->
                            sharedModulePayloads.astJsons
                                |> Array.get handle
                                |> Maybe.map Types.String
                                |> Maybe.withDefault (Types.String "")
                                |> Types.EvOk

                        _ ->
                            Types.EvOk (Types.String "")
                )
          )
        , ( "Review.Rule.finalCachePartsMarker"
          , Types.Intercept
                (\_ args _ _ ->
                    case args of
                        [ Types.String ruleName, Types.Int ruleId, splitCache ] ->
                            Types.EvYield "review-cache-parts-write"
                                (Types.Record
                                    (FastDict.fromList
                                        [ ( "ruleName", Types.String ruleName )
                                        , ( "ruleId", Types.Int ruleId )
                                        , ( "cache", splitCache )
                                        ]
                                    )
                                )
                                (\_ -> Types.EvOk splitCache)

                        _ ->
                            Types.EvOk (args |> List.reverse |> List.head |> Maybe.withDefault Types.Unit)
                )
          )
        , ( "Review.Cache.ContextHash.createContextHashMarker"
          , Types.Intercept
                (\_ args _ _ ->
                    case args of
                        [ context ] ->
                            -- Deep hash the context for cache key comparison
                            Types.EvOk (Types.Int (Eval.Expression.deepHashValue context))

                        _ ->
                            Types.EvOk Types.Unit
                )
          )
        , ( "ReviewRunnerHelper.projectCacheMarker"
          , Types.Intercept
                (\_ args _ _ ->
                    case args of
                        [ project ] ->
                            Types.EvYield "project-cache"
                                project
                                (\_ -> Types.EvOk project)

                        _ ->
                            Types.EvOk (args |> List.head |> Maybe.withDefault Types.Unit)
                )
          )
        , ( "Review.Cache.ContextHash.sort"
          , Types.Intercept
                (\_ args _ _ ->
                    case args of
                        [ Types.List items ] ->
                            -- Sort ContextHash values by their Int hash.
                            -- This makes ComparableContextHash deterministic
                            -- regardless of Dict.foldl order.
                            let
                                sortKey item =
                                    case item of
                                        Types.Custom _ [ Types.Int h ] ->
                                            h

                                        _ ->
                                            0
                            in
                            Types.EvOk (Types.List (List.sortBy sortKey items))

                        _ ->
                            -- Identity fallback
                            Types.EvOk (args |> List.head |> Maybe.withDefault Types.Unit)
                )
          )
        ]


{-| Load split rule caches from disk.

Base caches are stored as `<ruleKey>.base.json`.
Per-module entries are stored as `<ruleKey>--<safePath>.module.json`.
-}
loadRuleCachesWithStats : String -> List String -> BackendTask FatalError LoadedRuleCaches
loadRuleCachesWithStats buildDir modulePathsToLoad =
    let
        cacheDir =
            buildDir ++ "/rule-value-cache"

        decodeValueFile toKey filePath =
            File.rawFile filePath
                |> BackendTask.toResult
                |> BackendTask.map
                    (\content ->
                        case content of
                            Ok json ->
                                { entry =
                                    ValueCodec.decodeValue json
                                        |> Maybe.map (\value -> ( toKey filePath, value ))
                                , bytes = String.length json
                                }

                            Err _ ->
                                { entry = Nothing, bytes = 0 }
                    )
    in
    Glob.fromStringWithOptions
        (let
            o =
                Glob.defaultOptions
         in
         { o | include = Glob.OnlyFiles }
        )
        (cacheDir ++ "/*.base.json")
        |> BackendTask.toResult
        |> BackendTask.map
            (\result ->
                case result of
                    Ok files ->
                        files

                    Err _ ->
                        []
            )
        |> BackendTask.andThen
            (\baseFiles ->
                let
                    baseDecoders : List (BackendTask FatalError { entry : Maybe ( String, Types.Value ), bytes : Int })
                    baseDecoders =
                        baseFiles
                            |> List.map
                                (decodeValueFile
                                    (\filePath ->
                                        filePath
                                            |> String.split "/"
                                            |> List.reverse
                                            |> List.head
                                            |> Maybe.withDefault ""
                                            |> String.replace ".base.json" ""
                                    )
                                )

                    moduleFiles :
                        List
                            { ruleKey : String
                            , modulePath : String
                            , filePath : String
                            }
                    moduleFiles =
                        baseFiles
                            |> List.concatMap
                                (\baseFile ->
                                    let
                                        ruleKey =
                                            baseFile
                                                |> String.split "/"
                                                |> List.reverse
                                                |> List.head
                                                |> Maybe.withDefault ""
                                                |> String.replace ".base.json" ""
                                    in
                                    modulePathsToLoad
                                        |> List.map
                                            (\modulePath ->
                                                { ruleKey = ruleKey
                                                , modulePath = modulePath
                                                , filePath =
                                                    cacheDir
                                                        ++ "/"
                                                        ++ ruleKey
                                                        ++ "--"
                                                        ++ cacheFileComponent modulePath
                                                        ++ ".module.json"
                                                }
                                            )
                                )

                    moduleDecoders : List (BackendTask FatalError { entry : Maybe ( String, ( String, Types.Value ) ), bytes : Int })
                    moduleDecoders =
                        moduleFiles
                            |> List.map
                                (\moduleFile ->
                                    File.rawFile moduleFile.filePath
                                        |> BackendTask.toResult
                                        |> BackendTask.map
                                            (\content ->
                                                case content of
                                                    Ok json ->
                                                        { entry =
                                                            ValueCodec.decodeValue json
                                                                |> Maybe.map
                                                                    (\value ->
                                                                        ( moduleFile.ruleKey
                                                                        , ( moduleFile.modulePath, value )
                                                                        )
                                                                    )
                                                        , bytes = String.length json
                                                        }

                                                    Err _ ->
                                                        { entry = Nothing, bytes = 0 }
                                            )
                                )

                    baseTask : BackendTask FatalError (List { entry : Maybe ( String, Types.Value ), bytes : Int })
                    baseTask =
                        baseDecoders |> BackendTask.Extra.combine

                    moduleTask : BackendTask FatalError (List { entry : Maybe ( String, ( String, Types.Value ) ), bytes : Int })
                    moduleTask =
                        moduleDecoders |> BackendTask.Extra.combine

                    combineLoadedCaches :
                        List { entry : Maybe ( String, Types.Value ), bytes : Int }
                        -> List { entry : Maybe ( String, ( String, Types.Value ) ), bytes : Int }
                        -> LoadedRuleCaches
                    combineLoadedCaches baseResults moduleResults =
                        let
                            decodedBaseEntries =
                                baseResults
                                    |> List.filterMap .entry

                            decodedModuleEntryList =
                                moduleResults
                                    |> List.filterMap .entry

                            decodedModuleEntries =
                                decodedModuleEntryList
                                    |> List.foldl
                                        (\( ruleKey, ( modulePath, value ) ) acc ->
                                            Dict.update ruleKey
                                                (\existing ->
                                                    Just
                                                        (existing
                                                            |> Maybe.withDefault Dict.empty
                                                            |> Dict.insert modulePath value
                                                        )
                                                )
                                                acc
                                        )
                                        Dict.empty
                        in
                        { baseCaches = Dict.fromList decodedBaseEntries
                        , moduleEntries = decodedModuleEntries
                        , entryCount = List.length decodedBaseEntries + List.length decodedModuleEntryList
                        , bytes =
                            (baseResults |> List.map .bytes |> List.sum)
                                + (moduleResults |> List.map .bytes |> List.sum)
                        }
                in
                BackendTask.andThen
                    (\baseResults ->
                        BackendTask.map (combineLoadedCaches baseResults) moduleTask
                    )
                    baseTask
            )


{-| Save rule cache Values to disk after eval.
Extracts cache data from the returned updatedRules Value.
-}
saveRuleCaches : String -> Types.Value -> BackendTask FatalError ()
saveRuleCaches buildDir updatedRulesValue =
    let
        cacheDir =
            buildDir ++ "/rule-value-cache"

        -- The updatedRules is a List of Rule values. Each Rule contains
        -- a ProjectRuleCache. We serialize the ENTIRE updatedRules value
        -- as a single cache entry keyed by "all-rules".
        serialized =
            ValueCodec.encodeValue updatedRulesValue
    in
    Do.do (Script.exec "mkdir" [ "-p", cacheDir ]) <| \_ ->
    Script.writeFile { path = cacheDir ++ "/all-rules.json", body = serialized }
        |> BackendTask.allowFatal


persistProjectEvalOutput : Maybe ProjectEvalOutputCache -> String -> BackendTask FatalError String
persistProjectEvalOutput maybeOutputCache output =
    case maybeOutputCache of
        Just outputCache ->
            Do.do
                (Script.writeFile { path = outputCache.keyPath, body = outputCache.key } |> BackendTask.allowFatal)
            <| \_ ->
            Do.do
                (Script.writeFile { path = outputCache.dataPath, body = output } |> BackendTask.allowFatal)
            <| \_ ->
            BackendTask.succeed output

        Nothing ->
            BackendTask.succeed output


appendMemoProfileLog : Bool -> String -> String -> Int -> MemoRuntime.MemoStats -> BackendTask FatalError ()
appendMemoProfileLog enabled buildDir label memoEntryCount memoStats =
    if not enabled || memoStats.lookups <= 0 then
        BackendTask.succeed ()

    else
        let
            logPath =
                buildDir ++ "/memo-profile.log"

            section =
                String.join "\n"
                    [ "## " ++ label
                    , MemoRuntime.formatMemoStats memoEntryCount memoStats
                    , ""
                    ]
        in
        Do.do
            (File.rawFile logPath
                |> BackendTask.toResult
            )
        <| \existing ->
        Script.writeFile
            { path = logPath
            , body = Result.withDefault "" existing ++ section
            }
            |> BackendTask.allowFatal
            |> BackendTask.map (\_ -> ())


runProjectRulesFresh :
    InterpreterProject
    -> List Int
    -> List { path : String, source : String, astJson : String }
    -> String
runProjectRulesFresh reviewProject prIndices modulesWithAst =
    let
        modulesVarName =
            "reviewModules__"
    in
    case
        InterpreterProject.prepareAndEvalWithValues reviewProject
            { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
            , expression = buildExpressionForRulesWithModuleVar prIndices modulesVarName
            , sourceOverrides = [ reviewRunnerHelperSource ]
            , injectedValues =
                FastDict.singleton modulesVarName (moduleInputsValue modulesWithAst)
            }
    of
        Ok (Types.String output) ->
            output

        Ok _ ->
            ""

        Err err ->
            err


runProjectRulesFreshWithMemo :
    InterpreterProject
    -> List Int
    -> List { path : String, source : String, astJson : String }
    -> Set.Set String
    -> Bool
    -> MemoRuntime.MemoCache
    -> BackendTask FatalError
        { output : String
        , memoCache : MemoRuntime.MemoCache
        , memoStats : MemoRuntime.MemoStats
        }
runProjectRulesFreshWithMemo reviewProject prIndices modulesWithAst memoizedFunctions memoProfile memoCache =
    let
        modulesVarName =
            "reviewModules__"

        injectedValues =
            FastDict.singleton modulesVarName (moduleInputsValue modulesWithAst)
    in
    if Set.isEmpty memoizedFunctions then
        BackendTask.succeed ()
            |> BackendTask.map
                (\_ ->
                    { output = runProjectRulesFresh reviewProject prIndices modulesWithAst
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    }
                )

    else
        case
            InterpreterProject.prepareAndEvalWithValuesAndMemoizedFunctions reviewProject
                { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                , expression = buildExpressionForRulesWithModuleVar prIndices modulesVarName
                , sourceOverrides = [ reviewRunnerHelperSource ]
                , injectedValues = injectedValues
                , memoizedFunctions = memoizedFunctions
                , memoCache = memoCache
                , collectMemoStats = memoProfile
                }
        of
            Ok evalResult ->
                case evalResult.value of
                    Types.String output ->
                        BackendTask.succeed
                            { output = output
                            , memoCache = evalResult.memoCache
                            , memoStats = evalResult.memoStats
                            }

                    _ ->
                        BackendTask.succeed
                            { output = runProjectRulesFresh reviewProject prIndices modulesWithAst
                            , memoCache = memoCache
                            , memoStats = MemoRuntime.emptyMemoStats
                            }

            Err err ->
                BackendTask.succeed
                    { output = err
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    }


runProjectRulesWithCacheWrite :
    String
    -> InterpreterProject
    -> List Int
    -> List { path : String, source : String, astJson : String }
    -> List String
    -> Set.Set String
    -> Bool
    -> MemoRuntime.MemoCache
    -> BackendTask FatalError ProjectRuleEvalResult
runProjectRulesWithCacheWrite buildDir reviewProject prIndices modulesWithAst finalCachePathsToWrite memoizedFunctions memoProfile memoCache =
    let
        moduleHandlesVarName =
            "reviewModuleHandles__"

        expression =
            buildExpressionForRulesWithHandleVar prIndices moduleHandlesVarName

        sharedModuleHandlePayloads =
            moduleHandlePayloads modulesWithAst

        injectedValues =
            FastDict.singleton moduleHandlesVarName sharedModuleHandlePayloads.handlesValue

        fallbackOutput () =
            runProjectRulesFresh reviewProject prIndices modulesWithAst

        loadedRuleCaches =
            { baseCaches = Dict.empty
            , moduleEntries = Dict.empty
            , entryCount = 0
            , bytes = 0
            }

        intercepts =
            buildReviewIntercepts finalCachePathsToWrite sharedModuleHandlePayloads.payloads loadedRuleCaches
    in
    if Set.isEmpty memoizedFunctions then
        Do.do
            (deferTask
                (\() ->
                    InterpreterProject.prepareAndEvalWithYield reviewProject
                        { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                        , expression = expression
                        , sourceOverrides = [ reviewRunnerHelperSource ]
                        , intercepts = intercepts
                        , injectedValues = injectedValues
                        }
                        (projectRuleYieldHandler buildDir)
                )
            )
        <| \result ->
        case result of
            Ok (Types.String output) ->
                BackendTask.succeed
                    { output = output
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , loadedRuleCaches = loadedRuleCaches
                    , counters = Dict.empty
                    }

            Ok _ ->
                BackendTask.succeed
                    { output = fallbackOutput ()
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , loadedRuleCaches = loadedRuleCaches
                    , counters = Dict.empty
                    }

            Err _ ->
                BackendTask.succeed
                    { output = fallbackOutput ()
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , loadedRuleCaches = loadedRuleCaches
                    , counters = Dict.empty
                    }

    else
        Do.do
            (deferTask
                (\() ->
                    InterpreterProject.prepareAndEvalWithYieldAndMemoizedFunctions reviewProject
                        { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                        , expression = expression
                        , sourceOverrides = [ reviewRunnerHelperSource ]
                        , intercepts = intercepts
                        , injectedValues = injectedValues
                        , memoizedFunctions = memoizedFunctions
                        , memoCache = memoCache
                        , collectMemoStats = memoProfile
                        }
                        (projectRuleYieldHandler buildDir)
                )
            )
        <| \result ->
        case result.result of
            Ok (Types.String output) ->
                BackendTask.succeed
                    { output = output
                    , memoCache = result.memoCache
                    , memoStats = result.memoStats
                    , loadedRuleCaches = loadedRuleCaches
                    , counters = Dict.empty
                    }

            Ok _ ->
                BackendTask.succeed
                    { output = fallbackOutput ()
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , loadedRuleCaches = loadedRuleCaches
                    , counters = Dict.empty
                    }

            Err _ ->
                BackendTask.succeed
                    { output = fallbackOutput ()
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , loadedRuleCaches = loadedRuleCaches
                    , counters = Dict.empty
                    }


runProjectRulesWithWarmRuleCaches :
    String
    -> String
    -> InterpreterProject
    -> List Int
    -> List String
    -> List { path : String, source : String, astJson : String }
    -> List String
    -> Set.Set String
    -> Bool
    -> MemoRuntime.MemoCache
    -> BackendTask FatalError ProjectRuleEvalResult
runProjectRulesWithWarmRuleCaches counterPrefix buildDir reviewProject prIndices moduleCachePathsToLoad modulesWithAst finalCachePathsToWrite memoizedFunctions memoProfile memoCache =
    let
        moduleHandlesVarName =
            "reviewModuleHandles__"

        expression =
            buildExpressionForRulesWithHandleVar prIndices moduleHandlesVarName

        sharedModuleHandlePayloads =
            moduleHandlePayloads modulesWithAst

        injectedValues =
            FastDict.singleton moduleHandlesVarName sharedModuleHandlePayloads.handlesValue

        fallbackOutput () =
            runProjectRulesFresh reviewProject prIndices modulesWithAst
    in
    Do.do
        (withTiming (counterPrefix ++ ".rule_cache.load")
            (loadRuleCachesWithStats buildDir moduleCachePathsToLoad)
        )
    <| \loadTimed ->
    let
        loadedRuleCaches =
            loadTimed.value

        intercepts =
            buildReviewIntercepts finalCachePathsToWrite sharedModuleHandlePayloads.payloads loadedRuleCaches

        baseCounters =
            Dict.fromList
                [ ( counterPrefix ++ ".mode.fresh", 0 )
                , ( counterPrefix ++ ".mode.split", 1 )
                , ( counterPrefix ++ ".rule_cache.load_ms", loadTimed.stage.ms )
                ]
    in
    if Set.isEmpty memoizedFunctions then
        Do.do
            (withTiming (counterPrefix ++ ".warm_eval")
                (deferTask
                    (\() ->
                        InterpreterProject.prepareAndEvalWithYield reviewProject
                            { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                            , expression = expression
                            , sourceOverrides = [ reviewRunnerHelperSource ]
                            , intercepts = intercepts
                            , injectedValues = injectedValues
                            }
                            (projectRuleYieldHandler buildDir)
                    )
                )
            )
        <| \evalTimed ->
        case evalTimed.value of
            Ok (Types.String output) ->
                BackendTask.succeed
                    { output = output
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , loadedRuleCaches = loadedRuleCaches
                    , counters =
                        Dict.insert (counterPrefix ++ ".warm_eval_ms") evalTimed.stage.ms baseCounters
                    }

            Ok _ ->
                BackendTask.succeed
                    { output = fallbackOutput ()
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , loadedRuleCaches = loadedRuleCaches
                    , counters =
                        Dict.insert (counterPrefix ++ ".warm_eval_ms") evalTimed.stage.ms baseCounters
                    }

            Err _ ->
                BackendTask.succeed
                    { output = fallbackOutput ()
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , loadedRuleCaches = loadedRuleCaches
                    , counters =
                        Dict.insert (counterPrefix ++ ".warm_eval_ms") evalTimed.stage.ms baseCounters
                    }

    else
        Do.do
            (withTiming (counterPrefix ++ ".warm_eval")
                (deferTask
                    (\() ->
                        InterpreterProject.prepareAndEvalWithYieldAndMemoizedFunctions reviewProject
                            { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                            , expression = expression
                            , sourceOverrides = [ reviewRunnerHelperSource ]
                            , intercepts = intercepts
                            , injectedValues = injectedValues
                            , memoizedFunctions = memoizedFunctions
                            , memoCache = memoCache
                            , collectMemoStats = memoProfile
                            }
                            (projectRuleYieldHandler buildDir)
                    )
                )
            )
        <| \evalTimed ->
        case evalTimed.value.result of
            Ok (Types.String output) ->
                BackendTask.succeed
                    { output = output
                    , memoCache = evalTimed.value.memoCache
                    , memoStats = evalTimed.value.memoStats
                    , loadedRuleCaches = loadedRuleCaches
                    , counters =
                        Dict.insert (counterPrefix ++ ".warm_eval_ms") evalTimed.stage.ms baseCounters
                    }

            Ok _ ->
                BackendTask.succeed
                    { output = fallbackOutput ()
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , loadedRuleCaches = loadedRuleCaches
                    , counters =
                        Dict.insert (counterPrefix ++ ".warm_eval_ms") evalTimed.stage.ms baseCounters
                    }

            Err _ ->
                BackendTask.succeed
                    { output = fallbackOutput ()
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , loadedRuleCaches = loadedRuleCaches
                    , counters =
                        Dict.insert (counterPrefix ++ ".warm_eval_ms") evalTimed.stage.ms baseCounters
                    }


evalTwoProjectRuleSlices :
    InterpreterProject
    -> List Int
    -> List { path : String, source : String, astJson : String }
    -> List Int
    -> List { path : String, source : String, astJson : String }
    -> { firstOutput : String, secondOutput : String }
evalTwoProjectRuleSlices reviewProject firstIndices firstModules secondIndices secondModules =
    let
        indexList indices =
            "[ " ++ (indices |> List.map String.fromInt |> String.join ", ") ++ " ]"

        firstModulesVar =
            "firstModules__"

        secondModulesVar =
            "secondModules__"

        expression =
            "ReviewRunnerHelper.runTwoRuleGroups "
                ++ indexList firstIndices
                ++ " "
                ++ firstModulesVar
                ++ " "
                ++ indexList secondIndices
                ++ " "
                ++ secondModulesVar
    in
    case
        InterpreterProject.prepareAndEvalWithValues reviewProject
            { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
            , expression = expression
            , sourceOverrides = [ reviewRunnerHelperSource ]
            , injectedValues =
                FastDict.fromList
                    [ ( firstModulesVar, moduleInputsValue firstModules )
                    , ( secondModulesVar, moduleInputsValue secondModules )
                    ]
            }
    of
        Ok (Types.Tuple (Types.String firstOutput) (Types.String secondOutput)) ->
            { firstOutput = firstOutput, secondOutput = secondOutput }

        _ ->
            { firstOutput = runProjectRulesFresh reviewProject firstIndices firstModules
            , secondOutput = runProjectRulesFresh reviewProject secondIndices secondModules
            }


evalTwoProjectRuleSlicesWithMemo :
    InterpreterProject
    -> List Int
    -> List { path : String, source : String, astJson : String }
    -> List Int
    -> List { path : String, source : String, astJson : String }
    -> Set.Set String
    -> Bool
    -> MemoRuntime.MemoCache
    -> BackendTask FatalError
        { firstOutput : String
        , secondOutput : String
        , memoCache : MemoRuntime.MemoCache
        , memoStats : MemoRuntime.MemoStats
        }
evalTwoProjectRuleSlicesWithMemo reviewProject firstIndices firstModules secondIndices secondModules memoizedFunctions memoProfile memoCache =
    let
        indexList indices =
            "[ " ++ (indices |> List.map String.fromInt |> String.join ", ") ++ " ]"

        firstModulesVar =
            "firstModules__"

        secondModulesVar =
            "secondModules__"

        expression =
            "ReviewRunnerHelper.runTwoRuleGroups "
                ++ indexList firstIndices
                ++ " "
                ++ firstModulesVar
                ++ " "
                ++ indexList secondIndices
                ++ " "
                ++ secondModulesVar

        injectedValues =
            FastDict.fromList
                [ ( firstModulesVar, moduleInputsValue firstModules )
                , ( secondModulesVar, moduleInputsValue secondModules )
                ]
    in
    if Set.isEmpty memoizedFunctions then
        let
            result =
                evalTwoProjectRuleSlices reviewProject firstIndices firstModules secondIndices secondModules
        in
        BackendTask.succeed
            { firstOutput = result.firstOutput
            , secondOutput = result.secondOutput
            , memoCache = memoCache
            , memoStats = MemoRuntime.emptyMemoStats
            }

    else
        case
            InterpreterProject.prepareAndEvalWithValuesAndMemoizedFunctions reviewProject
                { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                , expression = expression
                , sourceOverrides = [ reviewRunnerHelperSource ]
                , injectedValues = injectedValues
                , memoizedFunctions = memoizedFunctions
                , memoCache = memoCache
                , collectMemoStats = memoProfile
                }
        of
            Ok evalResult ->
                case evalResult.value of
                    Types.Tuple (Types.String firstOutput) (Types.String secondOutput) ->
                        BackendTask.succeed
                            { firstOutput = firstOutput
                            , secondOutput = secondOutput
                            , memoCache = evalResult.memoCache
                            , memoStats = evalResult.memoStats
                            }

                    _ ->
                        Do.do (runProjectRulesFreshWithMemo reviewProject firstIndices firstModules memoizedFunctions memoProfile memoCache) <| \firstResult ->
                        Do.do (runProjectRulesFreshWithMemo reviewProject secondIndices secondModules memoizedFunctions memoProfile firstResult.memoCache) <| \secondResult ->
                        BackendTask.succeed
                            { firstOutput = firstResult.output
                            , secondOutput = secondResult.output
                            , memoCache = secondResult.memoCache
                            , memoStats = secondResult.memoStats
                            }

            Err _ ->
                Do.do (runProjectRulesFreshWithMemo reviewProject firstIndices firstModules memoizedFunctions memoProfile memoCache) <| \firstResult ->
                Do.do (runProjectRulesFreshWithMemo reviewProject secondIndices secondModules memoizedFunctions memoProfile firstResult.memoCache) <| \secondResult ->
                BackendTask.succeed
                    { firstOutput = firstResult.output
                    , secondOutput = secondResult.output
                    , memoCache = secondResult.memoCache
                    , memoStats = secondResult.memoStats
                    }


projectRuleYieldHandler : String -> String -> Types.Value -> BackendTask FatalError Types.Value
projectRuleYieldHandler buildDir tag payload =
    let
        ruleCacheDir =
            buildDir ++ "/rule-value-cache"
    in
    case tag of
        "log" ->
            BackendTask.succeed Types.Unit

        "review-cache-parts-write" ->
            case payload of
                Types.Record fields ->
                    let
                        ruleName =
                            FastDict.get "ruleName" fields
                                |> Maybe.andThen
                                    (\value ->
                                        case value of
                                            Types.String name ->
                                                Just name

                                            _ ->
                                                Nothing
                                    )
                                |> Maybe.withDefault "unknown"

                        ruleId =
                            FastDict.get "ruleId" fields
                                |> Maybe.andThen
                                    (\value ->
                                        case value of
                                            Types.Int id_ ->
                                                Just id_

                                            _ ->
                                                Nothing
                                    )
                                |> Maybe.withDefault 0

                        cache =
                            FastDict.get "cache" fields
                                |> Maybe.withDefault Types.Unit
                    in
                    case cache of
                        Types.Record splitFields ->
                            let
                                ruleKey =
                                    ruleCacheValueKey ruleName ruleId

                                baseCache =
                                    FastDict.get "baseCache" splitFields
                                        |> Maybe.withDefault Types.Unit

                                moduleContexts =
                                    FastDict.get "moduleContexts" splitFields
                                        |> Maybe.withDefault (Types.List [])

                                basePath =
                                    ruleCacheDir ++ "/" ++ ruleKey ++ ".base.json"
                            in
                            Do.do (Script.exec "mkdir" [ "-p", ruleCacheDir ]) <| \_ ->
                            Do.do
                                (Script.writeFile
                                    { path = basePath
                                    , body = ValueCodec.encodeValue baseCache
                                    }
                                    |> BackendTask.allowFatal
                                )
                            <| \_ ->
                            Do.do
                                (case moduleContexts of
                                    Types.List entries ->
                                        entries
                                            |> List.map
                                                (\entry ->
                                                    case entry of
                                                        Types.Tuple (Types.String modulePath) moduleCacheValue ->
                                                            Script.writeFile
                                                                { path =
                                                                    ruleCacheDir
                                                                        ++ "/"
                                                                        ++ ruleKey
                                                                        ++ "--"
                                                                        ++ cacheFileComponent modulePath
                                                                        ++ ".module.json"
                                                                , body = ValueCodec.encodeValue moduleCacheValue
                                                                }
                                                                |> BackendTask.allowFatal

                                                        _ ->
                                                            BackendTask.succeed ()
                                                )
                                            |> BackendTask.Extra.combine
                                            |> BackendTask.map (\_ -> ())

                                    _ ->
                                        BackendTask.succeed ()
                                )
                            <| \_ ->
                            BackendTask.succeed Types.Unit

                        _ ->
                            BackendTask.succeed Types.Unit

                _ ->
                    BackendTask.succeed Types.Unit

        "project-cache" ->
            Do.do
                (Script.writeFile
                    { path = buildDir ++ "/project-cache.json"
                    , body = ValueCodec.encodeValue payload
                    }
                    |> BackendTask.allowFatal
                )
            <| \_ ->
            BackendTask.succeed Types.Unit

        _ ->
            BackendTask.succeed Types.Unit


evalProjectRulesWithWarmState :
    Config
    -> InterpreterProject
    -> String
    -> List Int
    -> List AnalyzedTargetFile
    -> List AnalyzedTargetFile
    -> Maybe ProjectEvalOutputCache
    -> MemoRuntime.MemoCache
    ->
        BackendTask FatalError
            { output : String
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            , usedCachedProject : Bool
            }
evalProjectRulesWithWarmState config reviewProject helperHash prIndices allFileContents staleFileContents maybeOutputCache memoCache =
    let
        buildDir =
            Path.toString config.buildDirectory

        projectMetadata =
            projectCacheMetadataFor helperHash allFileContents

        allModulesWithAst =
            allFileContents
                |> List.map
                    (\file ->
                        { path = file.path
                        , source = file.source
                        , astJson = file.analysis.parsedAst.astJson
                        }
                    )

        staleModulesWithAst =
            staleFileContents
                |> List.map
                    (\file ->
                        { path = file.path
                        , source = file.source
                        , astJson = file.analysis.parsedAst.astJson
                        }
                    )

        indexList =
            "[ " ++ (prIndices |> List.map String.fromInt |> String.join ", ") ++ " ]"

        modulesVarName =
            "reviewModules__"
    in
    Do.do (loadRuleCachesWithStats buildDir (allFileContents |> List.map .path)) <| \preloadedCaches ->
    Do.do (loadProjectCache buildDir projectMetadata) <| \maybeCachedProject ->
    let
        intercepts =
            buildReviewIntercepts
                (allFileContents |> List.map .path)
                emptySharedModulePayloads
                preloadedCaches

        expression =
            case maybeCachedProject of
                Just _ ->
                    "ReviewRunnerHelper.runReviewWithCachedProject "
                        ++ indexList
                        ++ " cachedProj__ "
                        ++ modulesVarName

                Nothing ->
                    "ReviewRunnerHelper.runReviewCachingWithProject "
                        ++ indexList
                        ++ " "
                        ++ modulesVarName

        injectedValues =
            case maybeCachedProject of
                Just cachedProject ->
                    FastDict.fromList
                        [ ( "cachedProj__", cachedProject )
                        , ( modulesVarName, moduleInputsValue staleModulesWithAst )
                        ]

                Nothing ->
                    FastDict.singleton modulesVarName (moduleInputsValue allModulesWithAst)

        persistWarmOutput output =
            Do.do (persistProjectCacheMetadata buildDir projectMetadata) <| \_ ->
            persistProjectEvalOutput maybeOutputCache output

        fallbackOutput () =
            runProjectRulesFresh reviewProject prIndices allModulesWithAst

        usedCachedProject =
            case maybeCachedProject of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    if Set.isEmpty config.memoizedFunctions then
        Do.do
            (InterpreterProject.prepareAndEvalWithYield reviewProject
                { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                , expression = expression
                , sourceOverrides = [ reviewRunnerHelperSource ]
                , intercepts = intercepts
                , injectedValues = injectedValues
                }
                (projectRuleYieldHandler buildDir)
            )
        <| \result ->
        case result of
            Ok (Types.Tuple (Types.String output) _) ->
                Do.do (persistWarmOutput output) <| \_ ->
                BackendTask.succeed
                    { output = output
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , usedCachedProject = usedCachedProject
                    }

            Ok (Types.String output) ->
                Do.do (persistWarmOutput output) <| \_ ->
                BackendTask.succeed
                    { output = output
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , usedCachedProject = usedCachedProject
                    }

            Ok _ ->
                Do.do (persistProjectEvalOutput maybeOutputCache (fallbackOutput ())) <| \output ->
                BackendTask.succeed
                    { output = output
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , usedCachedProject = usedCachedProject
                    }

            Err _ ->
                Do.do (persistProjectEvalOutput maybeOutputCache (fallbackOutput ())) <| \output ->
                BackendTask.succeed
                    { output = output
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , usedCachedProject = usedCachedProject
                    }

    else
        Do.do
            (InterpreterProject.prepareAndEvalWithYieldAndMemoizedFunctions reviewProject
                { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                , expression = expression
                , sourceOverrides = [ reviewRunnerHelperSource ]
                , intercepts = intercepts
                , injectedValues = injectedValues
                , memoizedFunctions = config.memoizedFunctions
                , memoCache = memoCache
                , collectMemoStats = config.memoProfile
                }
                (projectRuleYieldHandler buildDir)
            )
        <| \result ->
        case result.result of
            Ok (Types.Tuple (Types.String output) _) ->
                Do.do (appendMemoProfileLog config.memoProfile buildDir "full-project-warm" (MemoRuntime.entryCount result.memoCache) result.memoStats) <| \_ ->
                Do.do (persistWarmOutput output) <| \_ ->
                BackendTask.succeed
                    { output = output
                    , memoCache = result.memoCache
                    , memoStats = result.memoStats
                    , usedCachedProject = usedCachedProject
                    }

            Ok (Types.String output) ->
                Do.do (appendMemoProfileLog config.memoProfile buildDir "full-project-warm" (MemoRuntime.entryCount result.memoCache) result.memoStats) <| \_ ->
                Do.do (persistWarmOutput output) <| \_ ->
                BackendTask.succeed
                    { output = output
                    , memoCache = result.memoCache
                    , memoStats = result.memoStats
                    , usedCachedProject = usedCachedProject
                    }

            Ok _ ->
                Do.do (persistProjectEvalOutput maybeOutputCache (fallbackOutput ())) <| \output ->
                BackendTask.succeed
                    { output = output
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , usedCachedProject = usedCachedProject
                    }

            Err _ ->
                Do.do (persistProjectEvalOutput maybeOutputCache (fallbackOutput ())) <| \output ->
                BackendTask.succeed
                    { output = output
                    , memoCache = memoCache
                    , memoStats = MemoRuntime.emptyMemoStats
                    , usedCachedProject = usedCachedProject
                    }


evalProjectRulesSingle :
    String
    -> Set.Set String
    -> Bool
    -> InterpreterProject
    -> List Int
    -> List { path : String, source : String, astJson : String }
    -> String
    -> String
    -> String
    -> BackendTask FatalError String
evalProjectRulesSingle buildDir memoizedFunctions memoProfile reviewProject prIndices modulesWithAst cacheKeyPath cacheDataPath cacheKey =
    Do.do (runProjectRulesFreshWithMemo reviewProject prIndices modulesWithAst memoizedFunctions memoProfile MemoRuntime.emptyMemoCache) <| \evalResult ->
    Do.do (appendMemoProfileLog memoProfile buildDir "project-rules-cold" (MemoRuntime.entryCount evalResult.memoCache) evalResult.memoStats) <| \_ ->
    persistProjectEvalOutput
        (Just
            { keyPath = cacheKeyPath
            , dataPath = cacheDataPath
            , key = cacheKey
            }
        )
        evalResult.output


{-| Two-pass project rule evaluation with Value injection (kept for reference).

Pass 1: Run all project rules with runReviewCaching to get (errors, updatedRules).
Pass 2: Re-run with cached rules on the same modules. elm-review's internal
ModuleCacheEntry skips unchanged modules (32/33 on warm-1-file).

The pass 2 result is the one we use (it has the warm cache benefit).
Both passes store errors to disk cache.
-}
evalProjectRulesTwoPass :
    Config
    -> InterpreterProject
    -> List Int
    -> List { path : String, source : String, astJson : String }
    -> String
    -> String
    -> String
    -> BackendTask FatalError String
evalProjectRulesTwoPass config reviewProject prIndices modulesWithAst cacheKeyPath cacheDataPath cacheKey =
    let
        moduleList =
            buildModuleListWithAst modulesWithAst

        -- Use ReviewRunnerHelper.runReviewCaching which returns (errorStr, updatedRules)
        -- but only for the selected project rule indices
        indexList =
            "[ " ++ (prIndices |> List.map String.fromInt |> String.join ", ") ++ " ]"

        -- Expression: select rules by index, build project, review, return (errors, rules) tuple
        cachingExpr =
            "ReviewRunnerHelper.runReviewCachingByIndices " ++ indexList ++ " " ++ moduleList
    in
    Do.do BackendTask.Time.now <| \t1 ->
    let
        pass1Result =
            InterpreterProject.prepareAndEvalRaw reviewProject
                { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                , expression = cachingExpr
                , sourceOverrides = [ reviewRunnerHelperSource ]
                }
    in
    case pass1Result of
        Ok (Types.Tuple (Types.String errorStr) rulesValue) ->
            Do.do BackendTask.Time.now <| \t2 ->
            Do.do (Script.log ("  project rules pass 1: " ++ String.fromInt (Time.posixToMillis t2 - Time.posixToMillis t1) ++ "ms")) <| \_ ->
            -- Pass 2: re-eval with cached rules (elm-review skips unchanged modules)
            let
                cachedExpr =
                    "ReviewRunnerHelper.runReviewWithCachedRules cachedRules__ " ++ moduleList

                pass2Result =
                    InterpreterProject.prepareAndEvalWithValues reviewProject
                        { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                        , expression = cachedExpr
                        , sourceOverrides = [ reviewRunnerHelperSource ]
                        , injectedValues = FastDict.singleton "cachedRules__" rulesValue
                        }
            in
            case pass2Result of
                Ok (Types.Tuple (Types.String errorStr2) _) ->
                    Do.do BackendTask.Time.now <| \t3 ->
                    Do.do (Script.log ("  project rules pass 2 (cached): " ++ String.fromInt (Time.posixToMillis t3 - Time.posixToMillis t2) ++ "ms")) <| \_ ->
                    -- Cache the pass 2 result (warm)
                    Do.do
                        (Script.writeFile { path = cacheKeyPath, body = cacheKey }
                            |> BackendTask.allowFatal
                        )
                    <| \_ ->
                    Do.do
                        (Script.writeFile { path = cacheDataPath, body = errorStr2 }
                            |> BackendTask.allowFatal
                        )
                    <| \_ ->
                    BackendTask.succeed errorStr2

                _ ->
                    -- Pass 2 failed, use pass 1 result
                    Do.do
                        (Script.writeFile { path = cacheKeyPath, body = cacheKey } |> BackendTask.allowFatal)
                    <| \_ ->
                    Do.do
                        (Script.writeFile { path = cacheDataPath, body = errorStr } |> BackendTask.allowFatal)
                    <| \_ ->
                    BackendTask.succeed errorStr

        Ok (Types.String errorStr) ->
            -- Got string directly (no tuple), store as-is
            Do.do
                (Script.writeFile { path = cacheKeyPath, body = cacheKey } |> BackendTask.allowFatal)
            <| \_ ->
            Do.do
                (Script.writeFile { path = cacheDataPath, body = errorStr } |> BackendTask.allowFatal)
            <| \_ ->
            BackendTask.succeed errorStr

        Ok _ ->
            BackendTask.succeed ""

        Err errStr ->
            BackendTask.succeed errStr


formatErrorLine : ReviewError -> String
formatErrorLine err =
    "RULE:" ++ err.ruleName ++ "|FILE:" ++ err.filePath ++ "|LINE:" ++ String.fromInt err.line ++ "|COL:" ++ String.fromInt err.column ++ "|MSG:" ++ err.message


{-| Get the number of rules in ReviewConfig.config by evaluating through the interpreter.
-}
getRuleCount : Config -> BackendTask FatalError Int
getRuleCount config =
    Do.do (loadReviewProject config) <| \reviewProject ->
    Cache.run { jobs = Nothing } config.buildDirectory
        (InterpreterProject.evalWithSourceOverrides reviewProject
            { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
            , expression = "String.fromInt ReviewRunnerHelper.ruleCount"
            , sourceOverrides = [ reviewRunnerHelperSource ]
            }
            Cache.succeed
        )
        |> BackendTask.andThen
            (\cacheResult ->
                File.rawFile (Path.toString cacheResult.output)
                    |> BackendTask.allowFatal
                    |> BackendTask.map
                        (\s ->
                            String.toInt (String.trim s)
                                |> Maybe.withDefault 0
                        )
            )


{-| Get rule names and count by evaluating through the interpreter.
Returns a list of (ruleIndex, ruleName, ruleType) for each rule in ReviewConfig.config.
Rule type is determined by matching the rule name against known module rule packages.
-}
getRuleInfo :
    Config
    -> BackendTask FatalError (List RuleInfo)
getRuleInfo config =
    Do.do (loadReviewProject config) <| \reviewProject ->
    getRuleInfoWithProject config reviewProject


getRuleInfoWithProject :
    Config
    -> InterpreterProject
    -> BackendTask FatalError (List RuleInfo)
getRuleInfoWithProject config reviewProject =
    Do.do (fileExists (ruleInfoCachePath config)) <| \cacheExists ->
    if cacheExists then
        Do.do
            (File.rawFile (ruleInfoCachePath config)
                |> BackendTask.allowFatal
            )
        <| \cachedJson ->
        case decodeRuleInfo cachedJson of
            Ok cachedRuleInfo ->
                BackendTask.succeed cachedRuleInfo

            Err _ ->
                computeAndPersistRuleInfo config reviewProject

    else
        computeAndPersistRuleInfo config reviewProject


computeAndPersistRuleInfo :
    Config
    -> InterpreterProject
    -> BackendTask FatalError (List RuleInfo)
computeAndPersistRuleInfo config reviewProject =
    Cache.run { jobs = Nothing } config.buildDirectory
        (InterpreterProject.evalWithSourceOverrides reviewProject
            { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
            , expression = "ReviewRunnerHelper.ruleNames"
            , sourceOverrides = [ reviewRunnerHelperSource ]
            }
            Cache.succeed
        )
        |> BackendTask.andThen
            (\cacheResult ->
                File.rawFile (Path.toString cacheResult.output)
                    |> BackendTask.allowFatal
                    |> BackendTask.map
                        (\namesStr ->
                            namesStr
                                |> String.split ","
                                |> List.indexedMap
                                    (\i name ->
                                        { index = i
                                        , name = name
                                        , ruleType = classifyByRuleName name
                                        }
                                    )
                        )
            )
        |> BackendTask.andThen
            (\ruleInfo ->
                Script.writeFile
                    { path = ruleInfoCachePath config
                    , body = encodeRuleInfo ruleInfo
                    }
                    |> BackendTask.allowFatal
                    |> BackendTask.map (\_ -> ruleInfo)
            )


ruleInfoCachePath : Config -> String
ruleInfoCachePath config =
    Path.toString config.buildDirectory ++ "/rule-info.json"


encodeRuleInfo : List RuleInfo -> String
encodeRuleInfo ruleInfo =
    ruleInfo
        |> Json.Encode.list
            (\info ->
                Json.Encode.object
                    [ ( "index", Json.Encode.int info.index )
                    , ( "name", Json.Encode.string info.name )
                    , ( "ruleType", Json.Encode.string (ruleTypeToString info.ruleType) )
                    ]
            )
        |> Json.Encode.encode 0


decodeRuleInfo : String -> Result String (List RuleInfo)
decodeRuleInfo json =
    let
        decoder : Json.Decode.Decoder (List RuleInfo)
        decoder =
            Json.Decode.list <|
                Json.Decode.map3
                    (\index name ruleType ->
                        { index = index
                        , name = name
                        , ruleType = ruleType
                        }
                    )
                    (Json.Decode.field "index" Json.Decode.int)
                    (Json.Decode.field "name" Json.Decode.string)
                    (Json.Decode.field "ruleType" ruleTypeDecoder)
    in
    Json.Decode.decodeString decoder json
        |> Result.mapError Json.Decode.errorToString


ruleTypeDecoder : Json.Decode.Decoder RuleType
ruleTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\raw ->
                case raw of
                    "module" ->
                        Json.Decode.succeed ModuleRule

                    "project" ->
                        Json.Decode.succeed ProjectRule

                    _ ->
                        Json.Decode.fail ("Unknown rule type: " ++ raw)
            )


ruleTypeToString : RuleType -> String
ruleTypeToString ruleType =
    case ruleType of
        ModuleRule ->
            "module"

        ProjectRule ->
            "project"


{-| Classify a rule by its name. Known module-rule packages are classified as ModuleRule.
Everything else defaults to ProjectRule (conservative).
-}
classifyByRuleName : String -> RuleType
classifyByRuleName name =
    let
        knownModuleRules =
            [ "NoDebug.Log"
            , "NoDebug.TodoOrToString"
            , "NoExposingEverything"
            , "NoImportingEverything"
            , "NoMissingTypeAnnotation"
            , "NoMissingTypeAnnotationInLetIn"
            , "NoUnused.Patterns"
            , "NoBooleanCase"
            , "NoConfusingPrefixOperator"
            , "NoDuplicatePorts"
            , "NoModuleOnExposedNames"
            , "NoPrematureLetComputation"
            , "NoRecursiveUpdate"
            , "NoSimpleLetBody"
            , "NoUnnecessaryTrailingUnderscore"
            , "NoRedundantlyQualifiedType"
            ]
    in
    if List.member name knownModuleRules then
        ModuleRule

    else
        ProjectRule


{-| Load the review project (shared between loadAndEval and loadAndEvalPerRule).
-}
loadReviewProject : Config -> BackendTask FatalError InterpreterProject
loadReviewProject config =
    loadReviewProjectDetailed config
        |> BackendTask.map .project


loadReviewProjectDetailed : Config -> BackendTask FatalError LoadedReviewProject
loadReviewProjectDetailed config =
    InterpreterProject.loadWithProfile
        { projectDir = Path.path config.reviewDir
        , skipPackages = Set.union kernelPackages conflictingPackages
        , patchSource = patchSource
        , extraSourceFiles = []
        , sourceDirectories = Just [ config.reviewDir ++ "/src" ]
        , packageParseCacheDir = Just (Path.toString config.buildDirectory)
        }
        |> BackendTask.map
            (\loaded ->
                { project = loaded.project
                , counters =
                    Dict.fromList
                        [ ( "load_review_project.resolve_source_directories_ms", loaded.profile.resolveSourceDirectoriesMs )
                        , ( "load_review_project.load_package_sources_ms", loaded.profile.loadPackageSourcesMs )
                        , ( "load_review_project.package_summary_cache_hit", loaded.profile.packageSummaryCacheHit )
                        , ( "load_review_project.package_summary_cache_roundtrip_ok", loaded.profile.packageSummaryCacheRoundtripOk )
                        , ( "load_review_project.package_summary_cache_bytes", loaded.profile.packageSummaryCacheBytes )
                        , ( "load_review_project.load_package_summary_cache_ms", loaded.profile.loadPackageSummaryCacheMs )
                        , ( "load_review_project.decode_package_summary_cache_ms", loaded.profile.decodePackageSummaryCacheMs )
                        , ( "load_review_project.validate_package_summary_cache_ms", loaded.profile.validatePackageSummaryCacheMs )
                        , ( "load_review_project.write_package_summary_cache_ms", loaded.profile.writePackageSummaryCacheMs )
                        , ( "load_review_project.glob_user_sources_ms", loaded.profile.globUserSourcesMs )
                        , ( "load_review_project.read_user_sources_ms", loaded.profile.readUserSourcesMs )
                        , ( "load_review_project.read_extra_sources_ms", loaded.profile.readExtraSourcesMs )
                        , ( "load_review_project.build_graph_ms", loaded.profile.buildGraphMs )
                        , ( "load_review_project.parse_package_sources_ms", loaded.profile.parsePackageSourcesMs )
                        , ( "load_review_project.build_package_summaries_from_parsed_ms", loaded.profile.buildPackageSummariesFromParsedMs )
                        , ( "load_review_project.build_package_env_from_summaries_ms", loaded.profile.buildPackageEnvFromSummariesMs )
                        , ( "load_review_project.build_package_env_ms", loaded.profile.buildPackageEnvMs )
                        , ( "load_review_project.build_base_user_env_ms", loaded.profile.buildBaseUserEnvMs )
                        , ( "load_review_project.build_semantic_index_ms", loaded.profile.buildSemanticIndexMs )
                        , ( "load_review_project.cache_inputs_ms", loaded.profile.cacheInputsMs )
                        ]
                }
            )


{-| Write the per-declaration cache to disk.
-}
persistCache : String -> CacheState -> BackendTask FatalError ()
persistCache path cache =
    Script.writeFile
        { path = path
        , body = encodeCacheState cache
        }
        |> BackendTask.allowFatal


{-| Report errors and exit with appropriate code.
-}
reportErrors : List ReviewError -> BackendTask FatalError ()
reportErrors errors =
    if List.isEmpty errors then
        Script.log (Ansi.Color.fontColor Ansi.Color.brightGreen "No errors found!")

    else
        Do.do (displayErrors errors) <| \_ ->
        Do.log
            (Ansi.Color.fontColor Ansi.Color.brightRed
                ("\nI found " ++ String.fromInt (List.length errors) ++ " error(s) in " ++ String.fromInt (countFiles errors) ++ " file(s).")
            )
        <| \_ ->
        BackendTask.fail
            (FatalError.build
                { title = ""
                , body = ""
                }
            )


countFiles : List ReviewError -> Int
countFiles errors =
    errors
        |> List.map .filePath
        |> Set.fromList
        |> Set.size


displayErrors : List ReviewError -> BackendTask FatalError ()
displayErrors errors =
    let
        -- Group errors by file
        byFile =
            errors
                |> List.foldl
                    (\err acc ->
                        Dict.update err.filePath
                            (\existing ->
                                case existing of
                                    Just list ->
                                        Just (list ++ [ err ])

                                    Nothing ->
                                        Just [ err ]
                            )
                            acc
                    )
                    Dict.empty
    in
    byFile
        |> Dict.toList
        |> List.concatMap
            (\( filePath, fileErrors ) ->
                fileErrors
                    |> List.map
                        (\err ->
                            Script.log
                                ("\n"
                                    ++ Ansi.Color.fontColor Ansi.Color.cyan
                                        ("-- " ++ String.toUpper err.ruleName ++ " ")
                                    ++ Ansi.Color.fontColor Ansi.Color.cyan
                                        (String.repeat (60 - String.length err.ruleName) "-"
                                            ++ " "
                                            ++ filePath
                                            ++ ":"
                                            ++ String.fromInt err.line
                                            ++ ":"
                                            ++ String.fromInt err.column
                                        )
                                    ++ "\n\n"
                                    ++ "    "
                                    ++ err.message
                                )
                        )
            )
        |> BackendTask.Extra.sequence_


resolveTargetFiles : Config -> BackendTask FatalError (List String)
resolveTargetFiles config =
    config.sourceDirs
        |> List.map (\dir -> dir ++ "/**/*.elm")
        |> List.map
            (\globPattern ->
                Glob.fromStringWithOptions
                    (let
                        o =
                            Glob.defaultOptions
                     in
                     { o | include = Glob.OnlyFiles }
                    )
                    globPattern
            )
        |> BackendTask.Extra.combine
        |> BackendTask.map (List.concat >> List.sort)


readTargetFiles : List String -> BackendTask FatalError (List { path : String, source : String })
readTargetFiles files =
    files
        |> List.map
            (\filePath ->
                File.rawFile filePath
                    |> BackendTask.allowFatal
                    |> BackendTask.map (\content -> { path = filePath, source = content })
            )
        |> BackendTask.Extra.combine


ensureReviewDeps : String -> BackendTask FatalError Bool
ensureReviewDeps reviewDir =
    Script.exec "elm" [ "make", "src/ReviewConfig.elm", "--output", "/dev/null" ]
        |> BackendTask.inDir reviewDir
        |> BackendTask.toResult
        |> BackendTask.map
            (\result ->
                case result of
                    Ok _ ->
                        True

                    Err _ ->
                        False
            )


kernelPackages : Set.Set String
kernelPackages =
    Set.fromList
        [ "elm/html"
        , "elm/virtual-dom"
        , "elm/browser"
        , "elm/http"
        , "elm/file"
        , "elm/url"
        ]


{-| Packages to skip because they have unsupported kernel code.
Module name collisions (multiple packages exposing `Util` etc.)
are a known limitation of the interpreter's flat namespace —
projects with conflicting packages may need to exclude some rules.
-}
conflictingPackages : Set.Set String
conflictingPackages =
    Set.fromList
        [ -- Util collisions
          "truqu/elm-review-nobooleancase"
        , "SiriusStarr/elm-review-no-single-pattern-case"
        , "SiriusStarr/elm-review-no-unsorted"
        , "lue-bird/elm-review-equals-caseable"
        , "lue-bird/elm-review-no-catch-all-for-specific-remaining-patterns"
        , "lue-bird/elm-review-variables-between-case-of-access-in-cases"
        , "lue-bird/elm-no-record-type-alias-constructor-function"

        -- Char.Extra / other collisions
        , "miniBill/elm-rope"
        , "gampleman/elm-review-derive"
        , "dillonkearns/elm-review-html-to-elm"
        , "matzko/elm-review-limit-aliased-record-size"
        , "sparksp/elm-review-camelcase"
        , "sparksp/elm-review-imports"
        , "sparksp/elm-review-ports"
        , "miniBill/elm-review-no-broken-elm-parser-functions"
        , "miniBill/elm-review-no-internal-imports"
        , "miniBill/elm-review-validate-regexes"
        , "folq/review-rgb-ranges"
        , "SiriusStarr/elm-review-pipeline-styles"
        , "vkfisher/elm-review-no-unsafe-division"
        , "lue-bird/elm-review-documentation-code-snippet"
        ]


patchSource : String -> String
patchSource source =
    let
        patchedKernelThunk =
            if String.contains "runThunk =\n    Elm.Kernel.Test.runThunk" source then
                source
                    |> String.replace
                        "runThunk =\n    Elm.Kernel.Test.runThunk"
                        "runThunk fn =\n    Ok (fn ())"

            else
                source

        reviewRuleCacheFilterOriginal =
            """removeUnknownModulesFromInitialCache : ValidProject -> ProjectRuleCache projectContext -> ProjectRuleCache projectContext
removeUnknownModulesFromInitialCache validProject projectRuleCache =
    { projectRuleCache | moduleContexts = Dict.filter (\\path _ -> ValidProject.doesModuleExist path validProject) projectRuleCache.moduleContexts }
"""

        reviewRuleCacheFilterPatched =
            """initialCacheAllowedPathsMarker : () -> List String
initialCacheAllowedPathsMarker _ =
    []


removeUnknownModulesFromInitialCache : ValidProject -> ProjectRuleCache projectContext -> ProjectRuleCache projectContext
removeUnknownModulesFromInitialCache validProject projectRuleCache =
    let
        allowedPaths =
            initialCacheAllowedPathsMarker ()
    in
    if List.isEmpty allowedPaths then
        { projectRuleCache | moduleContexts = Dict.filter (\\path _ -> ValidProject.doesModuleExist path validProject) projectRuleCache.moduleContexts }

    else
        { projectRuleCache | moduleContexts = Dict.filter (\\path _ -> List.member path allowedPaths) projectRuleCache.moduleContexts }
"""

        initialCacheMarkerOriginal =
            """initialCacheMarker : String -> Int -> ProjectRuleCache projectContext -> ProjectRuleCache projectContext
initialCacheMarker _ _ cache =
    cache
"""

        initialCacheMarkerPatched =
            """initialCachePartsMarker : String -> Int -> SplitProjectRuleCache projectContext -> SplitProjectRuleCache projectContext
initialCachePartsMarker _ _ splitCache =
    splitCache


initialCacheMarker : String -> Int -> ProjectRuleCache projectContext -> ProjectRuleCache projectContext
initialCacheMarker ruleName ruleId cache =
    initialCachePartsMarker ruleName ruleId (splitProjectRuleCache cache)
        |> rehydrateProjectRuleCache
"""

        finalCacheMarkerOriginal =
            """finalCacheMarker : String -> Int -> ProjectRuleCache projectContext -> ProjectRuleCache projectContext
finalCacheMarker _ _ cache =
    cache
"""

        finalCacheMarkerPatched =
            """finalCachePartsMarker : String -> Int -> SplitProjectRuleCache projectContext -> SplitProjectRuleCache projectContext
finalCachePartsMarker _ _ splitCache =
    splitCache


finalCacheAllowedPathsMarker : () -> List String
finalCacheAllowedPathsMarker _ =
    []


finalCacheMarker : String -> Int -> ProjectRuleCache projectContext -> ProjectRuleCache projectContext
finalCacheMarker ruleName ruleId cache =
    finalCachePartsMarker ruleName ruleId (splitProjectRuleCacheToPaths (finalCacheAllowedPathsMarker ()) cache)
        |> rehydrateProjectRuleCache
"""

        projectRuleCacheOriginal =
            """type alias ProjectRuleCache projectContext =
    { elmJson : Maybe (ProjectFileCache projectContext)
    , readme : Maybe (ProjectFileCache projectContext)
    , extraFiles : Maybe (ExtraFilesCache projectContext)
    , dependencies : Maybe (ProjectFileCache projectContext)
    , moduleContexts : Dict String (ModuleCacheEntry projectContext)
    , finalEvaluationErrors : Maybe (FinalProjectEvaluationCache projectContext)
    , extract : Maybe (ExtractCache projectContext)
    }
"""

        projectRuleCachePatched =
            """type alias ProjectRuleCache projectContext =
    { elmJson : Maybe (ProjectFileCache projectContext)
    , readme : Maybe (ProjectFileCache projectContext)
    , extraFiles : Maybe (ExtraFilesCache projectContext)
    , dependencies : Maybe (ProjectFileCache projectContext)
    , moduleContexts : Dict String (ModuleCacheEntry projectContext)
    , finalEvaluationErrors : Maybe (FinalProjectEvaluationCache projectContext)
    , extract : Maybe (ExtractCache projectContext)
    }


type alias SplitProjectRuleCache projectContext =
    { baseCache : ProjectRuleCache projectContext
    , moduleContexts : List ( String, ModuleCacheEntry projectContext )
    }


splitProjectRuleCache : ProjectRuleCache projectContext -> SplitProjectRuleCache projectContext
splitProjectRuleCache cache =
    { baseCache = { cache | moduleContexts = Dict.empty }
    , moduleContexts = Dict.toList cache.moduleContexts
    }


splitProjectRuleCacheToPaths : List String -> ProjectRuleCache projectContext -> SplitProjectRuleCache projectContext
splitProjectRuleCacheToPaths allowedPaths cache =
    if List.isEmpty allowedPaths then
        splitProjectRuleCache cache

    else
        { baseCache = { cache | moduleContexts = Dict.empty }
        , moduleContexts =
            cache.moduleContexts
                |> Dict.filter (\\path _ -> List.member path allowedPaths)
                |> Dict.toList
        }


rehydrateProjectRuleCache : SplitProjectRuleCache projectContext -> ProjectRuleCache projectContext
rehydrateProjectRuleCache splitCache =
    let
        baseCache =
            splitCache.baseCache
    in
    { baseCache | moduleContexts = Dict.fromList splitCache.moduleContexts }
"""

        patchedReviewRuleCacheFilter =
            if String.contains reviewRuleCacheFilterOriginal patchedKernelThunk then
                patchedKernelThunk
                    |> String.replace reviewRuleCacheFilterOriginal reviewRuleCacheFilterPatched

            else
                patchedKernelThunk

        patchedInitialCacheMarker =
            if String.contains initialCacheMarkerOriginal patchedReviewRuleCacheFilter then
                patchedReviewRuleCacheFilter
                    |> String.replace initialCacheMarkerOriginal initialCacheMarkerPatched

            else
                patchedReviewRuleCacheFilter

        patchedFinalCacheMarker =
            if String.contains finalCacheMarkerOriginal patchedInitialCacheMarker then
                patchedInitialCacheMarker
                    |> String.replace finalCacheMarkerOriginal finalCacheMarkerPatched

            else
                patchedInitialCacheMarker
    in
    if String.contains projectRuleCacheOriginal patchedFinalCacheMarker then
        patchedFinalCacheMarker
            |> String.replace projectRuleCacheOriginal projectRuleCachePatched

    else
        patchedFinalCacheMarker
