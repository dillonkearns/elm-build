module ReviewRunner exposing (CacheDecision(..), CacheState, CrossModuleDep(..), DeclarationCache, ReviewError, RuleDependencyProfile, RuleType(..), buildExpression, buildExpressionForRule, buildExpressionForRules, buildExpressionWithAst, buildModuleRecord, checkCache, classifyRuleSource, computeSemanticKey, decodeCacheState, encodeCacheState, encodeFileAsJson, escapeElmString, getDeclarationHashes, mapErrorsToDeclarations, narrowCacheKey, parseReviewOutput, profileForRule, reviewRunnerHelperSource, run, updateCache)

{-| Run elm-review rules via the interpreter.

Usage:
    npx elm-pages run src/ReviewRunner.elm

-}

import Ansi.Color
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
    }


type alias DeclarationHashInfo =
    { name : String
    , semanticHash : String
    , startLine : Int
    , endLine : Int
    }


type alias FileAnalysis =
    { sourceHash : String
    , astJson : String
    , aspectHashes : SemanticHash.FileAspectHashes
    , declarations : List DeclarationHashInfo
    , moduleName : String
    , imports : List String
    }


type alias AnalyzedTargetFile =
    { path : String
    , source : String
    , analysis : FileAnalysis
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


cacheSchemaVersion : String
cacheSchemaVersion =
    "review-runner-v4"


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
            )


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
                    DepGraph.reverseDeps depGraph filePath
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
        |> List.map (\file -> file.path ++ "|" ++ file.analysis.aspectHashes.fullHash)
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
                                file.analysis.declarations
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
                                        file.analysis.aspectHashes.fullHash

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
                        mapErrorsToDeclarations file.analysis.declarations fileErrors

                    fileCacheWithAspects =
                        Dict.insert "__fileAspects__"
                            { semanticHash = file.analysis.aspectHashes.fullHash, errors = [] }
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


encodeFileAnalysisCache : Dict String FileAnalysis -> String
encodeFileAnalysisCache cache =
    cache
        |> Dict.toList
        |> Json.Encode.list
            (\( filePath, analysis ) ->
                Json.Encode.object
                    [ ( "file", Json.Encode.string filePath )
                    , ( "sourceHash", Json.Encode.string analysis.sourceHash )
                    , ( "astJson", Json.Encode.string analysis.astJson )
                    , ( "aspectHashes", encodeFileAspectHashes analysis.aspectHashes )
                    , ( "moduleName", Json.Encode.string analysis.moduleName )
                    , ( "imports", Json.Encode.list Json.Encode.string analysis.imports )
                    , ( "declarations"
                      , analysis.declarations
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
            Json.Decode.map7
                (\filePath sourceHash astJson aspectHashes moduleName imports declarations ->
                    ( filePath
                    , { sourceHash = sourceHash
                      , astJson = astJson
                      , aspectHashes = aspectHashes
                      , declarations = declarations
                      , moduleName = moduleName
                      , imports = imports
                      }
                    )
                )
                (Json.Decode.field "file" Json.Decode.string)
                (Json.Decode.field "sourceHash" Json.Decode.string)
                (Json.Decode.field "astJson" Json.Decode.string)
                (Json.Decode.field "aspectHashes" fileAspectHashesDecoder)
                (Json.Decode.field "moduleName" Json.Decode.string)
                (Json.Decode.field "imports" (Json.Decode.list Json.Decode.string))
                (Json.Decode.field "declarations" (Json.Decode.list declarationDecoder))
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
            Just
                { sourceHash = hashSourceContents source
                , astJson =
                    Elm.Syntax.File.encode file
                        |> Json.Encode.encode 0
                , aspectHashes = SemanticHash.computeAspectHashesFromFile file
                , declarations = getDeclarationHashesFromFile file
                , moduleName = moduleNameFromFile file
                , imports = importsFromFile file
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


loadAnalyzedTargetFiles : Config -> List String -> BackendTask FatalError (List AnalyzedTargetFile)
loadAnalyzedTargetFiles config files =
    let
        cachePath =
            Path.toString config.buildDirectory ++ "/review-file-analysis-cache.json"
    in
    Do.do
        (File.rawFile cachePath
            |> BackendTask.toResult
            |> BackendTask.map
                (\result ->
                    result
                        |> Result.toMaybe
                        |> Maybe.andThen decodeFileAnalysisCache
                        |> Maybe.withDefault Dict.empty
                )
        )
    <| \previousCache ->
    Do.do (readTargetFiles files) <| \targetFileContents ->
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
    in
    Do.do
        (if cacheMissed || cachePathsChanged then
            persistFileAnalysisCache cachePath updatedCache

         else
            BackendTask.succeed ()
        )
    <| \_ ->
    BackendTask.succeed analyzedFiles


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
        [ "module ReviewRunnerHelper exposing (buildProject, extractRuleCaches, ruleCount, ruleNames, runReview, runReviewCaching, runReviewCachingByIndices, runReviewCachingWithProject, runReviewWithCachedProject, runReviewWithCachedRules, runRulesByIndices, runSingleRule)"
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
    Do.do (prepareConfig config) <| \preparedConfig ->
    let
        declCachePath =
            Path.toString preparedConfig.buildDirectory ++ "/review-decl-cache.json"
    in
    -- Load previous per-declaration cache from disk
    Do.do
        (File.rawFile declCachePath
            |> BackendTask.toResult
            |> BackendTask.map
                (\result ->
                    result
                        |> Result.toMaybe
                        |> Maybe.andThen decodeCacheState
                        |> Maybe.withDefault Dict.empty
                )
        )
    <| \previousCache ->
    Do.do (resolveTargetFiles preparedConfig) <| \targetFiles ->
    Do.do (loadAnalyzedTargetFiles preparedConfig targetFiles) <| \targetFileContents ->
    let
        decision =
            checkCacheWithAnalyses previousCache targetFileContents
    in
    case decision of
        FullCacheHit errors ->
            -- All declarations cached — skip interpreter entirely
            reportErrors errors

        ColdMiss _ ->
            -- No cache — treat all files as stale (same code path as PartialMiss)
            Do.do (loadReviewProject preparedConfig) <| \reviewProject ->
            Do.do (getRuleInfoWithProject preparedConfig reviewProject) <| \ruleInfo ->
            Do.do (loadAndEvalHybridPartialWithProject preparedConfig reviewProject ruleInfo targetFileContents targetFileContents []) <| \output ->
            let
                errors =
                    parseReviewOutput output

                newCache =
                    updateCacheWithAnalyses Dict.empty targetFileContents errors
            in
            Do.do (persistCache declCachePath newCache) <| \_ ->
            reportErrors errors

        PartialMiss { cachedErrors, staleFiles } ->
            -- Some files changed. Module rules on unchanged files use cachedErrors.
            -- Only stale files need module rule re-eval. Project rules always re-eval.
            Do.do (loadReviewProject preparedConfig) <| \reviewProject ->
            Do.do (getRuleInfoWithProject preparedConfig reviewProject) <| \ruleInfo ->
            let
                staleFileContents =
                    targetFileContents |> List.filter (\f -> List.member f.path staleFiles)
            in
            Do.do (loadAndEvalHybridPartialWithProject preparedConfig reviewProject ruleInfo targetFileContents staleFileContents cachedErrors) <| \output ->
            let
                freshErrors =
                    parseReviewOutput output

                newCache =
                    updateCacheWithAnalyses previousCache targetFileContents freshErrors
            in
            Do.do (persistCache declCachePath newCache) <| \_ ->
            reportErrors freshErrors


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
    -> BackendTask FatalError String
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
    -> BackendTask FatalError String
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
                        , astJson = file.analysis.astJson
                        }
                    )

        allModulesWithAst =
            allFileContents
                |> List.map
                    (\file ->
                        { path = file.path
                        , source = file.source
                        , astJson = file.analysis.astJson
                        }
                    )

        -- Per-file aspect hashes for narrow project rule keys
        allFileAspectHashes : Dict String SemanticHash.FileAspectHashes
        allFileAspectHashes =
            allFileContents
                |> List.map
                    (\file ->
                        ( file.path, file.analysis.aspectHashes )
                    )
                |> Dict.fromList

        depGraph : DepGraph.Graph
        depGraph =
            DepGraph.buildGraphFromModuleData
                (allFileContents
                    |> List.map
                        (\file ->
                            { filePath = file.path
                            , moduleName = file.analysis.moduleName
                            , imports = file.analysis.imports
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
                                        |> Maybe.withDefault file.analysis.aspectHashes

                                moduleWithAst =
                                    { path = file.path
                                    , source = file.source
                                    , astJson = file.analysis.astJson
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
    in
    -- Step 1: Run module rules on STALE files only
    Cache.run { jobs = Nothing } config.buildDirectory allMonads
        |> BackendTask.andThen
            (\cacheResult ->
                -- Read stale module rule results
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
                            -- Combine: cached unchanged + fresh stale
                            [ cachedModuleRuleErrors, freshModuleErrors ]
                                |> List.filter (not << String.isEmpty)
                                |> String.join "\n"
                        )
            )
        |> BackendTask.andThen
            (\moduleRuleOutput ->
                -- Step 2: Project rules — split by profile into two groups:
                -- Group A (ImportersOf): per-file narrow keys, skip if all hit
                -- Group B (FullProject): global key, always re-eval on any change
                if List.isEmpty projectRules then
                    BackendTask.succeed moduleRuleOutput

                else
                    let
                        importersOfRules =
                            projectRules
                                |> List.filter
                                    (\rule ->
                                        case (profileForRule rule.name).crossModuleDep of
                                            ImportersOf ->
                                                True

                                            _ ->
                                                False
                                    )

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

                        prCacheDir =
                            Path.toString config.buildDirectory ++ "/pr-per-file"

                        -- Combined profile for ImportersOf rules only
                        importersProfile =
                            mergeProfiles ImportersOf importersOfRules

                        -- Combined profile for DependenciesOf rules
                        dependenciesProfile =
                            mergeProfiles DependenciesOf dependenciesOfRules

                        -- Per-file narrow keys for ImportersOf rules
                        importersPerFileKeys =
                            allFileContents
                                |> List.map
                                    (\file ->
                                        let
                                            fileHashes =
                                                Dict.get file.path allFileAspectHashes
                                                    |> Maybe.withDefault file.analysis.aspectHashes
                                        in
                                        { path = file.path
                                        , narrowKey =
                                            "prf-io|" ++ helperHash ++ "|"
                                                ++ (importersOfRules |> List.map (.index >> String.fromInt) |> String.join ",")
                                                ++ "|" ++ narrowCacheKey importersProfile file.path fileHashes allFileAspectHashes depGraph
                                        }
                                    )

                        -- Per-file narrow keys for DependenciesOf rules
                        depsPerFileKeys =
                            allFileContents
                                |> List.map
                                    (\file ->
                                        let
                                            fileHashes =
                                                Dict.get file.path allFileAspectHashes
                                                    |> Maybe.withDefault file.analysis.aspectHashes
                                        in
                                        { path = file.path
                                        , narrowKey =
                                            "prf-do|" ++ helperHash ++ "|"
                                                ++ (dependenciesOfRules |> List.map (.index >> String.fromInt) |> String.join ",")
                                                ++ "|" ++ narrowCacheKey dependenciesProfile file.path fileHashes allFileAspectHashes depGraph
                                        }
                                    )
                    in
                    -- Check ImportersOf per-file caches
                    Do.do (Script.exec "mkdir" [ "-p", prCacheDir ]) <| \_ ->
                    Do.do
                        (if List.isEmpty importersOfRules then
                            BackendTask.succeed { allHit = True, outputs = "", fpIncluded = False }

                         else
                            Do.do
                                (importersPerFileKeys
                                    |> List.map
                                        (\{ path, narrowKey } ->
                                            let
                                                safePath =
                                                    path |> String.replace "/" "_"
                                            in
                                            File.rawFile (prCacheDir ++ "/" ++ safePath ++ ".key")
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
                                )
                            <| \checks ->
                            if List.all .hit checks then
                                -- All ImportersOf per-file caches valid
                                Do.do
                                    (importersPerFileKeys
                                        |> List.map
                                            (\{ path } ->
                                                File.rawFile (prCacheDir ++ "/" ++ (path |> String.replace "/" "_") ++ ".txt")
                                                    |> BackendTask.allowFatal
                                            )
                                        |> BackendTask.Extra.combine
                                    )
                                <| \cached ->
                                BackendTask.succeed
                                    { allHit = True
                                    , fpIncluded = False
                                    , outputs =
                                        cached
                                            |> List.filter (\s -> not (String.isEmpty (String.trim s)))
                                            |> String.join "\n"
                                    }

                            else
                                -- Some ImportersOf files missed — only re-eval
                                -- the affected reverse-dependency slice.
                                let
                                    ioIndices =
                                        importersOfRules |> List.map .index

                                    missedPaths =
                                        checks
                                            |> List.filter (not << .hit)
                                            |> List.map .path

                                    affectedEvalPaths =
                                        missedPaths
                                            |> List.foldl
                                                (\path acc ->
                                                    Set.union acc (DepGraph.reverseDeps depGraph path)
                                                )
                                                Set.empty

                                    affectedModules =
                                        allModulesWithAst
                                            |> List.filter (\file -> Set.member file.path affectedEvalPaths)

                                    result =
                                        InterpreterProject.prepareAndEval reviewProject
                                            { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                                            , expression = buildExpressionForRules ioIndices affectedModules
                                            , sourceOverrides = [ reviewRunnerHelperSource ]
                                            }

                                    ioOutput =
                                        case result of
                                            Ok s ->
                                                s

                                            Err e ->
                                                e

                                    ioErrors =
                                        parseReviewOutput ioOutput

                                    freshMissOutputs =
                                        missedPaths
                                            |> List.map
                                                (\path ->
                                                    ioErrors
                                                        |> List.filter (\err -> err.filePath == path)
                                                        |> List.map formatErrorLine
                                                        |> String.join "\n"
                                                )
                                in
                                Do.do
                                    (checks
                                        |> List.filterMap
                                            (\check ->
                                                if check.hit then
                                                    Just
                                                        (File.rawFile (prCacheDir ++ "/" ++ cacheFileComponent check.path ++ ".txt")
                                                            |> BackendTask.allowFatal
                                                        )

                                                else
                                                    Nothing
                                            )
                                        |> BackendTask.Extra.combine
                                    )
                                <| \cachedHitOutputs ->
                                Do.do
                                    (importersPerFileKeys
                                        |> List.filter (\{ path } -> List.member path missedPaths)
                                        |> List.map
                                            (\{ path, narrowKey } ->
                                                let
                                                    safePath =
                                                        cacheFileComponent path

                                                    fileErrors =
                                                        ioErrors
                                                            |> List.filter (\err -> err.filePath == path)
                                                            |> List.map formatErrorLine
                                                            |> String.join "\n"
                                                in
                                                Script.writeFile { path = prCacheDir ++ "/" ++ safePath ++ ".key", body = narrowKey }
                                                    |> BackendTask.allowFatal
                                                    |> BackendTask.andThen
                                                        (\_ ->
                                                            Script.writeFile { path = prCacheDir ++ "/" ++ safePath ++ ".txt", body = fileErrors }
                                                                |> BackendTask.allowFatal
                                                        )
                                            )
                                        |> BackendTask.Extra.combine
                                    )
                                <| \_ ->
                                BackendTask.succeed
                                    { allHit = False
                                    , fpIncluded = False
                                    , outputs =
                                        cachedHitOutputs
                                            ++ freshMissOutputs
                                            |> List.filter (\s -> not (String.isEmpty (String.trim s)))
                                            |> String.join "\n"
                                    }
                        )
                    <| \importersResult ->
                    -- Check DependenciesOf per-file caches (same pattern as ImportersOf)
                    let
                        doCacheDir =
                            Path.toString config.buildDirectory ++ "/pr-deps-of"
                    in
                    Do.do (Script.exec "mkdir" [ "-p", doCacheDir ]) <| \_ ->
                    Do.do
                        (if List.isEmpty dependenciesOfRules then
                            BackendTask.succeed { allHit = True, outputs = "", fpIncluded = False }

                         else
                            Do.do
                                (depsPerFileKeys
                                    |> List.map
                                        (\{ path, narrowKey } ->
                                            let
                                                safePath =
                                                    path |> String.replace "/" "_"
                                            in
                                            File.rawFile (doCacheDir ++ "/" ++ safePath ++ ".key")
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
                                )
                            <| \checks ->
                            if List.all .hit checks then
                                -- All DependenciesOf per-file caches valid
                                Do.do
                                    (depsPerFileKeys
                                        |> List.map
                                            (\{ path } ->
                                                File.rawFile (doCacheDir ++ "/" ++ (path |> String.replace "/" "_") ++ ".txt")
                                                    |> BackendTask.allowFatal
                                            )
                                        |> BackendTask.Extra.combine
                                    )
                                <| \cached ->
                                BackendTask.succeed
                                    { allHit = True
                                    , fpIncluded = False
                                    , outputs =
                                        cached
                                            |> List.filter (\s -> not (String.isEmpty (String.trim s)))
                                            |> String.join "\n"
                                    }

                            else
                                -- Some DependenciesOf files missed — only re-eval
                                -- the affected dependency slice.
                                let
                                    doIndices =
                                        dependenciesOfRules |> List.map .index

                                    missedPaths =
                                        checks
                                            |> List.filter (not << .hit)
                                            |> List.map .path

                                    affectedEvalPaths =
                                        missedPaths
                                            |> List.foldl
                                                (\path acc ->
                                                    Set.union acc (DepGraph.transitiveDeps depGraph path)
                                                )
                                                Set.empty

                                    affectedModules =
                                        allModulesWithAst
                                            |> List.filter (\file -> Set.member file.path affectedEvalPaths)

                                    doResult =
                                        InterpreterProject.prepareAndEval reviewProject
                                            { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                                            , expression = buildExpressionForRules doIndices affectedModules
                                            , sourceOverrides = [ reviewRunnerHelperSource ]
                                            }

                                    doOutput =
                                        case doResult of
                                            Ok s ->
                                                s

                                            Err e ->
                                                e

                                    doErrors =
                                        parseReviewOutput doOutput

                                    freshMissOutputs =
                                        missedPaths
                                            |> List.map
                                                (\path ->
                                                    doErrors
                                                        |> List.filter (\err -> err.filePath == path)
                                                        |> List.map formatErrorLine
                                                        |> String.join "\n"
                                                )
                                in
                                Do.do
                                    (checks
                                        |> List.filterMap
                                            (\check ->
                                                if check.hit then
                                                    Just
                                                        (File.rawFile (doCacheDir ++ "/" ++ cacheFileComponent check.path ++ ".txt")
                                                            |> BackendTask.allowFatal
                                                        )

                                                else
                                                    Nothing
                                            )
                                        |> BackendTask.Extra.combine
                                    )
                                <| \cachedHitOutputs ->
                                Do.do
                                    (depsPerFileKeys
                                        |> List.filter (\{ path } -> List.member path missedPaths)
                                        |> List.map
                                            (\{ path, narrowKey } ->
                                                let
                                                    safePath =
                                                        cacheFileComponent path

                                                    fileErrors =
                                                        doErrors
                                                            |> List.filter (\err -> err.filePath == path)
                                                            |> List.map formatErrorLine
                                                            |> String.join "\n"
                                                in
                                                Script.writeFile { path = doCacheDir ++ "/" ++ safePath ++ ".key", body = narrowKey }
                                                    |> BackendTask.allowFatal
                                                    |> BackendTask.andThen
                                                        (\_ ->
                                                            Script.writeFile { path = doCacheDir ++ "/" ++ safePath ++ ".txt", body = fileErrors }
                                                                |> BackendTask.allowFatal
                                                        )
                                            )
                                        |> BackendTask.Extra.combine
                                    )
                                <| \_ ->
                                BackendTask.succeed
                                    { allHit = False
                                    , outputs =
                                        cachedHitOutputs
                                            ++ freshMissOutputs
                                            |> List.filter (\s -> not (String.isEmpty (String.trim s)))
                                            |> String.join "\n"
                                    , fpIncluded = False
                                    }
                        )
                    <| \depsOfResult ->
                    -- Handle DependenciesOf + FullProject rules together.
                    -- When DependenciesOf missed, we already ran its eval above.
                    -- Now check FullProject separately.
                    let
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
                    in
                    if List.isEmpty fullProjectRules || depsOfResult.fpIncluded then
                        -- FullProject rules were already included in the combined DepsOf eval
                        -- or there are no FullProject rules. Just return combined outputs.
                        BackendTask.succeed
                            ([ moduleRuleOutput, importersResult.outputs, depsOfResult.outputs ]
                                |> List.filter (not << String.isEmpty)
                                |> String.join "\n"
                            )

                    else
                        evalProjectRulesWithWarmState
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
                            |> BackendTask.map
                                (\fpOutput ->
                                    [ moduleRuleOutput, importersResult.outputs, depsOfResult.outputs, fpOutput ]
                                        |> List.filter (\s -> not (String.isEmpty (String.trim s)) && not (String.startsWith "ERROR:" s))
                                        |> String.join "\n"
                                )
            )


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
                                                                evalProjectRulesSingle reviewProject prIndices modulesWithAst prCacheKeyPath prCacheDataPath prCacheKey
                                                    )

                                        else
                                            evalProjectRulesSingle reviewProject prIndices modulesWithAst prCacheKeyPath prCacheDataPath prCacheKey

                                    Err _ ->
                                        evalProjectRulesSingle reviewProject prIndices modulesWithAst prCacheKeyPath prCacheDataPath prCacheKey
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


buildReviewIntercepts :
    Dict String Types.Value
    -> FastDict.Dict String Types.Intercept
buildReviewIntercepts preloadedCaches =
    FastDict.fromList
        [ ( "Review.Rule.initialCacheMarker"
          , Types.Intercept
                (\args _ _ ->
                    case args of
                        [ Types.String ruleName, Types.Int ruleId, defaultCache ] ->
                            case Dict.get (ruleCacheValueKey ruleName ruleId) preloadedCaches of
                                Just cached ->
                                    Types.EvOk cached

                                Nothing ->
                                    Types.EvOk defaultCache

                        _ ->
                            Types.EvOk (args |> List.reverse |> List.head |> Maybe.withDefault Types.Unit)
                )
          )
        , ( "Review.Rule.finalCacheMarker"
          , Types.Intercept
                (\args _ _ ->
                    case args of
                        [ Types.String ruleName, Types.Int ruleId, cache ] ->
                            -- Yield to save cache to disk
                            Types.EvYield "review-cache-write"
                                (Types.Record
                                    (FastDict.fromList
                                        [ ( "ruleName", Types.String ruleName )
                                        , ( "ruleId", Types.Int ruleId )
                                        , ( "cache", cache )
                                        ]
                                    )
                                )
                                (\_ -> Types.EvOk cache)

                        _ ->
                            Types.EvOk (args |> List.reverse |> List.head |> Maybe.withDefault Types.Unit)
                )
          )
        , ( "Review.Cache.ContextHash.createContextHashMarker"
          , Types.Intercept
                (\args _ _ ->
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
                (\args _ _ ->
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
                (\args _ _ ->
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


{-| Load previously serialized rule cache Values from disk.
Returns a Dict keyed by "ruleName-ruleId".
-}
loadRuleCaches : String -> BackendTask FatalError (Dict String Types.Value)
loadRuleCaches buildDir =
    let
        cacheDir =
            buildDir ++ "/rule-value-cache"
    in
    Glob.fromStringWithOptions
        (let
            o =
                Glob.defaultOptions
         in
         { o | include = Glob.OnlyFiles }
        )
        (cacheDir ++ "/*.json")
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
            (\files ->
                files
                    |> List.map
                        (\filePath ->
                            File.rawFile filePath
                                |> BackendTask.toResult
                                |> BackendTask.map
                                    (\content ->
                                        case content of
                                            Ok json ->
                                                ValueCodec.decodeValue json
                                                    |> Maybe.map
                                                        (\value ->
                                                            let
                                                                -- Extract key from filename: "RuleName-0.json" -> "RuleName-0"
                                                                key =
                                                                    filePath
                                                                        |> String.split "/"
                                                                        |> List.reverse
                                                                        |> List.head
                                                                        |> Maybe.withDefault ""
                                                                        |> String.replace ".json" ""
                                                            in
                                                            Just ( key, value )
                                                        )
                                                    |> Maybe.withDefault Nothing

                                            Err _ ->
                                                Nothing
                                    )
                        )
                    |> BackendTask.Extra.combine
                    |> BackendTask.map
                        (\results ->
                            results
                                |> List.filterMap identity
                                |> Dict.fromList
                        )
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


runProjectRulesFresh :
    InterpreterProject
    -> List Int
    -> List { path : String, source : String, astJson : String }
    -> String
runProjectRulesFresh reviewProject prIndices modulesWithAst =
    case
        InterpreterProject.prepareAndEval reviewProject
            { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
            , expression = buildExpressionForRules prIndices modulesWithAst
            , sourceOverrides = [ reviewRunnerHelperSource ]
            }
    of
        Ok output ->
            output

        Err err ->
            err


projectRuleYieldHandler : String -> String -> Types.Value -> BackendTask FatalError Types.Value
projectRuleYieldHandler buildDir tag payload =
    let
        yieldLogPath =
            buildDir ++ "/yield-log.txt"

        ruleCacheDir =
            buildDir ++ "/rule-value-cache"
    in
    case tag of
        "log" ->
            case payload of
                Types.String msg ->
                    Do.do
                        (File.rawFile yieldLogPath
                            |> BackendTask.toResult
                            |> BackendTask.andThen
                                (\existing ->
                                    Script.writeFile
                                        { path = yieldLogPath
                                        , body = (Result.withDefault "" existing) ++ msg ++ "\n"
                                        }
                                        |> BackendTask.allowFatal
                                )
                        )
                    <| \_ ->
                    BackendTask.succeed Types.Unit

                _ ->
                    BackendTask.succeed Types.Unit

        "review-cache-write" ->
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

                        serialized =
                            ValueCodec.encodeValue cache

                        filePath =
                            ruleCacheDir ++ "/" ++ ruleCacheValueKey ruleName ruleId ++ ".json"
                    in
                    Do.do
                        (File.rawFile yieldLogPath
                            |> BackendTask.toResult
                            |> BackendTask.andThen
                                (\existing ->
                                    let
                                        prev =
                                            case existing of
                                                Ok s ->
                                                    s

                                                Err _ ->
                                                    ""
                                    in
                                    Script.writeFile
                                        { path = yieldLogPath
                                        , body =
                                            prev
                                                ++ ruleCacheValueKey ruleName ruleId
                                                ++ ": "
                                                ++ String.fromInt (String.length serialized)
                                                ++ " bytes\n"
                                        }
                                        |> BackendTask.allowFatal
                                )
                        )
                    <| \_ ->
                    Do.do (Script.exec "mkdir" [ "-p", ruleCacheDir ]) <| \_ ->
                    Do.do (Script.writeFile { path = filePath, body = serialized } |> BackendTask.allowFatal) <| \_ ->
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

        other ->
            Do.do
                (File.rawFile yieldLogPath
                    |> BackendTask.toResult
                    |> BackendTask.andThen
                        (\existing ->
                            Script.writeFile
                                { path = yieldLogPath
                                , body = (Result.withDefault "" existing) ++ "UNKNOWN yield: " ++ other ++ "\n"
                                }
                                |> BackendTask.allowFatal
                        )
                )
            <| \_ ->
            BackendTask.succeed Types.Unit


evalProjectRulesWithWarmState :
    Config
    -> InterpreterProject
    -> String
    -> List Int
    -> List AnalyzedTargetFile
    -> List AnalyzedTargetFile
    -> Maybe ProjectEvalOutputCache
    -> BackendTask FatalError String
evalProjectRulesWithWarmState config reviewProject helperHash prIndices allFileContents staleFileContents maybeOutputCache =
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
                        , astJson = file.analysis.astJson
                        }
                    )

        staleModulesWithAst =
            staleFileContents
                |> List.map
                    (\file ->
                        { path = file.path
                        , source = file.source
                        , astJson = file.analysis.astJson
                        }
                    )

        indexList =
            "[ " ++ (prIndices |> List.map String.fromInt |> String.join ", ") ++ " ]"
    in
    Do.do (loadRuleCaches buildDir) <| \preloadedCaches ->
    Do.do (loadProjectCache buildDir projectMetadata) <| \maybeCachedProject ->
    let
        intercepts =
            buildReviewIntercepts preloadedCaches

        expression =
            case maybeCachedProject of
                Just _ ->
                    "ReviewRunnerHelper.runReviewWithCachedProject "
                        ++ indexList
                        ++ " cachedProj__ "
                        ++ buildModuleListWithAst staleModulesWithAst

                Nothing ->
                    "ReviewRunnerHelper.runReviewCachingWithProject "
                        ++ indexList
                        ++ " "
                        ++ buildModuleListWithAst allModulesWithAst

        injectedValues =
            case maybeCachedProject of
                Just cachedProject ->
                    FastDict.singleton "cachedProj__" cachedProject

                Nothing ->
                    FastDict.empty

        persistWarmOutput output =
            Do.do (persistProjectCacheMetadata buildDir projectMetadata) <| \_ ->
            persistProjectEvalOutput maybeOutputCache output

        fallbackOutput =
            runProjectRulesFresh reviewProject prIndices allModulesWithAst
    in
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
            persistWarmOutput output

        Ok (Types.String output) ->
            persistWarmOutput output

        Ok _ ->
            persistProjectEvalOutput maybeOutputCache fallbackOutput

        Err _ ->
            persistProjectEvalOutput maybeOutputCache fallbackOutput


evalProjectRulesSingle :
    InterpreterProject
    -> List Int
    -> List { path : String, source : String, astJson : String }
    -> String
    -> String
    -> String
    -> BackendTask FatalError String
evalProjectRulesSingle reviewProject prIndices modulesWithAst cacheKeyPath cacheDataPath cacheKey =
    runProjectRulesFresh reviewProject prIndices modulesWithAst
        |> persistProjectEvalOutput
            (Just
                { keyPath = cacheKeyPath
                , dataPath = cacheDataPath
                , key = cacheKey
                }
            )


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
    -> BackendTask FatalError (List { index : Int, name : String, ruleType : RuleType })
getRuleInfo config =
    Do.do (loadReviewProject config) <| \reviewProject ->
    getRuleInfoWithProject config reviewProject


getRuleInfoWithProject :
    Config
    -> InterpreterProject
    -> BackendTask FatalError (List { index : Int, name : String, ruleType : RuleType })
getRuleInfoWithProject config reviewProject =
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
    InterpreterProject.loadWith
        { projectDir = Path.path config.reviewDir
        , skipPackages = Set.union kernelPackages conflictingPackages
        , patchSource = patchSource
        , extraSourceFiles = []
        , sourceDirectories = Just [ config.reviewDir ++ "/src" ]
        }


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
    if String.contains "runThunk =\n    Elm.Kernel.Test.runThunk" source then
        source
            |> String.replace
                "runThunk =\n    Elm.Kernel.Test.runThunk"
                "runThunk fn =\n    Ok (fn ())"

    else
        source
