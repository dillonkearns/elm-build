module ReviewRunner exposing (CacheDecision(..), CacheState, CrossModuleDep(..), DeclarationCache, ReviewError, RuleDependencyProfile, RuleType(..), buildExpression, buildExpressionForRule, buildExpressionWithAst, buildModuleRecord, checkCache, classifyRuleSource, computeSemanticKey, decodeCacheState, encodeCacheState, encodeFileAsJson, escapeElmString, getDeclarationHashes, mapErrorsToDeclarations, narrowCacheKey, parseReviewOutput, profileForRule, reviewRunnerHelperSource, run, updateCache)

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

                FullProject ->
                    allFileHashes
                        |> Dict.toList
                        |> List.sortBy Tuple.first
                        |> List.map (\( _, h ) -> h.fullHash)
                        |> String.join "|"
                        |> FNV1a.hash
                        |> String.fromInt
    in
    localKey ++ "|" ++ crossModuleKey


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


{-| Per-declaration cache entry: maps each top-level declaration
to its semantic hash and any review errors within its range.
-}
type alias DeclarationCache =
    Dict String { semanticHash : String, errors : List ReviewError }


{-| Get per-declaration semantic hashes and line ranges for a source file.
Returns (declarationName, semanticHash, startLine, endLine) for each
top-level function declaration.
-}
getDeclarationHashes : String -> List { name : String, semanticHash : String, startLine : Int, endLine : Int }
getDeclarationHashes source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            let
                index =
                    SemanticHash.buildIndexFromSource source

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
        [ "module ReviewRunnerHelper exposing (buildProject, ruleCount, ruleNames, runReview, runReviewCaching, runReviewCachingByIndices, runReviewCachingWithProject, runReviewWithCachedRules, runRulesByIndices, runSingleRule)"
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
        , "        project = projectCacheMarker (buildProject modules)"
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
    let
        declCachePath =
            Path.toString config.buildDirectory ++ "/review-decl-cache.json"
    in
    Do.do (ensureReviewDeps config.reviewDir) <| \_ ->
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
    Do.do (resolveTargetFiles config) <| \targetFiles ->
    Do.do (readTargetFiles targetFiles) <| \targetFileContents ->
    let
        decision =
            checkCache previousCache targetFileContents
    in
    case decision of
        FullCacheHit errors ->
            -- All declarations cached — skip interpreter entirely
            reportErrors errors

        ColdMiss _ ->
            -- No cache — treat all files as stale (same code path as PartialMiss)
            Do.do (getRuleInfo config) <| \ruleInfo ->
            Do.do (loadAndEvalHybridPartial config ruleInfo targetFileContents targetFileContents []) <| \output ->
            let
                errors =
                    parseReviewOutput output

                newCache =
                    updateCache Dict.empty targetFileContents errors
            in
            Do.do (persistCache declCachePath newCache) <| \_ ->
            reportErrors errors

        PartialMiss { cachedErrors, staleFiles } ->
            -- Some files changed. Module rules on unchanged files use cachedErrors.
            -- Only stale files need module rule re-eval. Project rules always re-eval.
            Do.do (getRuleInfo config) <| \ruleInfo ->
            let
                staleFileContents =
                    targetFileContents |> List.filter (\f -> List.member f.path staleFiles)
            in
            Do.do (loadAndEvalHybridPartial config ruleInfo targetFileContents staleFileContents cachedErrors) <| \output ->
            let
                freshErrors =
                    parseReviewOutput output

                newCache =
                    updateCache previousCache targetFileContents freshErrors
            in
            Do.do (persistCache declCachePath newCache) <| \_ ->
            reportErrors freshErrors


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
    -> List { path : String, source : String }
    -> List { path : String, source : String }
    -> List ReviewError
    -> BackendTask FatalError String
loadAndEvalHybridPartial config ruleInfo allFileContents staleFileContents cachedErrors =
    Do.do (loadReviewProject config) <| \reviewProject ->
    let
        helperHash =
            FNV1a.hash reviewRunnerHelperSource |> String.fromInt

        projectSemanticKey =
            computeSemanticKey allFileContents

        staleModulesWithAst =
            staleFileContents
                |> List.filterMap
                    (\{ path, source } ->
                        encodeFileAsJson source
                            |> Maybe.map (\astJson -> { path = path, source = source, astJson = astJson })
                    )

        allModulesWithAst =
            allFileContents
                |> List.filterMap
                    (\{ path, source } ->
                        encodeFileAsJson source
                            |> Maybe.map (\astJson -> { path = path, source = source, astJson = astJson })
                    )

        -- Per-file aspect hashes for narrow project rule keys
        allFileAspectHashes : Dict String SemanticHash.FileAspectHashes
        allFileAspectHashes =
            allFileContents
                |> List.map
                    (\{ path, source } ->
                        ( path, SemanticHash.computeAspectHashesFromSource source )
                    )
                |> Dict.fromList

        depGraph : DepGraph.Graph
        depGraph =
            DepGraph.buildGraph
                { sourceDirectories = config.sourceDirs
                , files =
                    allFileContents
                        |> List.map (\{ path, source } -> { filePath = path, content = source })
                }

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

        -- Module rules: only eval STALE files (others are cached)
        moduleRuleMonads =
            moduleRules
                |> List.concatMap
                    (\rule ->
                        let
                            profile =
                                profileForRule rule.name
                        in
                        staleModulesWithAst
                            |> List.indexedMap
                                (\fileIdx file ->
                                    let
                                        fileHashes =
                                            SemanticHash.computeAspectHashesFromSource file.source

                                        fileNarrowKey =
                                            narrowCacheKey profile file.path fileHashes Dict.empty (DepGraph.buildGraph { sourceDirectories = [], files = [] })

                                        cacheKey =
                                            "mr|" ++ String.fromInt rule.index ++ "|" ++ helperHash ++ "|" ++ file.path ++ "|" ++ fileNarrowKey
                                    in
                                    Cache.do (Cache.writeFile cacheKey Cache.succeed) <| \keyHash ->
                                    Cache.compute [ "mr", String.fromInt rule.index, file.path ]
                                        keyHash
                                        (\() ->
                                            case
                                                InterpreterProject.prepareAndEval reviewProject
                                                    { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                                                    , expression = buildExpressionForRule rule.index [ file ]
                                                    , sourceOverrides = [ reviewRunnerHelperSource ]
                                                    }
                                            of
                                                Ok s -> s
                                                Err e -> e
                                        )
                                    <| \hash ->
                                    Cache.succeed
                                        { filename = Path.path ("smr-" ++ String.fromInt rule.index ++ "-" ++ String.fromInt fileIdx)
                                        , hash = hash
                                        }
                                )
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
                    staleCount =
                        List.length staleModulesWithAst

                    mrFiles =
                        moduleRules
                            |> List.concatMap
                                (\rule ->
                                    List.range 0 (staleCount - 1)
                                        |> List.map
                                            (\fileIdx ->
                                                File.rawFile
                                                    (Path.toString cacheResult.output ++ "/smr-" ++ String.fromInt rule.index ++ "-" ++ String.fromInt fileIdx)
                                                    |> BackendTask.allowFatal
                                            )
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

                        fullProjectRules =
                            projectRules
                                |> List.filter
                                    (\rule ->
                                        case (profileForRule rule.name).crossModuleDep of
                                            ImportersOf ->
                                                False

                                            _ ->
                                                True
                                    )

                        prCacheDir =
                            Path.toString config.buildDirectory ++ "/pr-per-file"

                        -- Combined profile for ImportersOf rules only
                        importersProfile =
                            importersOfRules
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
                                    { expressionDep = False, declarationNameDep = False, importDep = False, exposingDep = False, customTypeDep = False, crossModuleDep = ImportersOf }

                        -- Per-file narrow keys for ImportersOf rules
                        importersPerFileKeys =
                            allFileContents
                                |> List.map
                                    (\file ->
                                        let
                                            fileHashes =
                                                Dict.get file.path allFileAspectHashes
                                                    |> Maybe.withDefault (SemanticHash.computeAspectHashesFromSource file.source)
                                        in
                                        { path = file.path
                                        , narrowKey =
                                            "prf-io|" ++ helperHash ++ "|"
                                                ++ (importersOfRules |> List.map (.index >> String.fromInt) |> String.join ",")
                                                ++ "|" ++ narrowCacheKey importersProfile file.path fileHashes allFileAspectHashes depGraph
                                        }
                                    )
                    in
                    -- Check ImportersOf per-file caches
                    Do.do (Script.exec "mkdir" [ "-p", prCacheDir ]) <| \_ ->
                    Do.do
                        (if List.isEmpty importersOfRules then
                            BackendTask.succeed { allHit = True, outputs = "" }

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
                                    , outputs =
                                        cached
                                            |> List.filter (\s -> not (String.isEmpty (String.trim s)))
                                            |> String.join "\n"
                                    }

                            else
                                -- Some ImportersOf files missed — eval ImportersOf rules
                                let
                                    ioIndices =
                                        importersOfRules |> List.map .index

                                    result =
                                        InterpreterProject.prepareAndEval reviewProject
                                            { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                                            , expression = buildExpressionForRules ioIndices allModulesWithAst
                                            , sourceOverrides = [ reviewRunnerHelperSource ]
                                            }

                                    ioOutput =
                                        case result of
                                            Ok s -> s
                                            Err e -> e

                                    ioErrors =
                                        parseReviewOutput ioOutput
                                in
                                -- Write per-file caches for ImportersOf rules
                                Do.do
                                    (importersPerFileKeys
                                        |> List.map
                                            (\{ path, narrowKey } ->
                                                let
                                                    safePath =
                                                        path |> String.replace "/" "_"

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
                                BackendTask.succeed { allHit = False, outputs = ioOutput }
                        )
                    <| \importersResult ->
                    -- Now handle FullProject rules (always re-eval on any change)
                    let
                        fpIndices =
                            fullProjectRules |> List.map .index

                        fpCacheKey =
                            "pr-fp|" ++ helperHash ++ "|" ++ (fpIndices |> List.map String.fromInt |> String.join ",") ++ "|" ++ projectSemanticKey

                        fpKeyPath =
                            Path.toString config.buildDirectory ++ "/pr-fp-cache.key"

                        fpDataPath =
                            Path.toString config.buildDirectory ++ "/pr-fp-cache.txt"
                    in
                    if List.isEmpty fullProjectRules then
                        BackendTask.succeed
                            ([ moduleRuleOutput, importersResult.outputs ]
                                |> List.filter (not << String.isEmpty)
                                |> String.join "\n"
                            )

                    else
                        -- Load any previously cached rule Values from disk
                        Do.do (loadRuleCaches (Path.toString config.buildDirectory)) <| \preloadedCaches ->
                        let
                            intercepts =
                                buildReviewIntercepts preloadedCaches

                            moduleList =
                                buildModuleListWithAst allModulesWithAst

                            -- Use runReviewCachingWithProject to get (errors, updatedRules, project)
                            -- The project Value is yielded for disk caching.
                            cachingExpr =
                                "ReviewRunnerHelper.runReviewCachingWithProject "
                                    ++ "[ " ++ (fpIndices |> List.map String.fromInt |> String.join ", ") ++ " ] "
                                    ++ moduleList

                            ruleCacheDir =
                                Path.toString config.buildDirectory ++ "/rule-value-cache"

                            -- Yield handler
                            yieldHandler tag payload =
                                case tag of
                                    "log" ->
                                        case payload of
                                            Types.String msg ->
                                                Do.do
                                                    (File.rawFile (Path.toString config.buildDirectory ++ "/yield-log.txt")
                                                        |> BackendTask.toResult
                                                        |> BackendTask.andThen
                                                            (\existing ->
                                                                Script.writeFile
                                                                    { path = Path.toString config.buildDirectory ++ "/yield-log.txt"
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
                                                                (\v ->
                                                                    case v of
                                                                        Types.String s ->
                                                                            Just s

                                                                        _ ->
                                                                            Nothing
                                                                )
                                                            |> Maybe.withDefault "unknown"

                                                    cache =
                                                        FastDict.get "cache" fields
                                                            |> Maybe.withDefault Types.Unit

                                                    serialized =
                                                        ValueCodec.encodeValue cache

                                                    filePath =
                                                        ruleCacheDir ++ "/" ++ ruleName ++ ".json"
                                                in
                                                Do.do
                                                    (File.rawFile (Path.toString config.buildDirectory ++ "/yield-log.txt")
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
                                                                    { path = Path.toString config.buildDirectory ++ "/yield-log.txt"
                                                                    , body = prev ++ ruleName ++ ": " ++ String.fromInt (String.length serialized) ++ " bytes\n"
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
                                        let
                                            serializedProject =
                                                ValueCodec.encodeValue payload

                                            projectPath =
                                                Path.toString config.buildDirectory ++ "/project-cache.json"
                                        in
                                        Do.do (Script.writeFile { path = projectPath, body = serializedProject } |> BackendTask.allowFatal) <| \_ ->
                                        BackendTask.succeed Types.Unit

                                    _ ->
                                        BackendTask.succeed Types.Unit
                        in
                        Do.do
                            (InterpreterProject.prepareAndEvalWithYield reviewProject
                                { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                                , expression = cachingExpr
                                , sourceOverrides = [ reviewRunnerHelperSource ]
                                , intercepts = intercepts
                                }
                                yieldHandler
                            )
                        <| \result ->
                        case result of
                            Ok (Types.Tuple (Types.String errorStr) _) ->
                                Do.do (Script.writeFile { path = fpKeyPath, body = fpCacheKey } |> BackendTask.allowFatal) <| \_ ->
                                Do.do (Script.writeFile { path = fpDataPath, body = errorStr } |> BackendTask.allowFatal) <| \_ ->
                                BackendTask.succeed
                                    ([ moduleRuleOutput, importersResult.outputs, errorStr ]
                                        |> List.filter (\s -> not (String.isEmpty (String.trim s)) && not (String.startsWith "ERROR:" s))
                                        |> String.join "\n"
                                    )

                            Ok (Types.String errorStr) ->
                                Do.do (Script.writeFile { path = fpKeyPath, body = fpCacheKey } |> BackendTask.allowFatal) <| \_ ->
                                Do.do (Script.writeFile { path = fpDataPath, body = errorStr } |> BackendTask.allowFatal) <| \_ ->
                                BackendTask.succeed
                                    ([ moduleRuleOutput, importersResult.outputs, errorStr ]
                                        |> List.filter (\s -> not (String.isEmpty (String.trim s)) && not (String.startsWith "ERROR:" s))
                                        |> String.join "\n"
                                    )

                            Ok otherValue ->
                                -- Debug: write the result type to a file
                                let
                                    typeStr =
                                        case otherValue of
                                            Types.Triple _ _ _ ->
                                                "Triple (non-string first)"

                                            Types.Tuple _ _ ->
                                                "Tuple (non-string first)"

                                            Types.Record _ ->
                                                "Record"

                                            Types.Custom ref _ ->
                                                "Custom " ++ ref.name

                                            _ ->
                                                "Other"
                                in
                                Do.do
                                    (Script.writeFile
                                        { path = Path.toString config.buildDirectory ++ "/result-type-debug.txt"
                                        , body = typeStr
                                        }
                                        |> BackendTask.allowFatal
                                    )
                                <| \_ ->
                                File.rawFile fpKeyPath
                                    |> BackendTask.toResult
                                    |> BackendTask.andThen
                                        (\keyResult ->
                                            case keyResult of
                                                Ok storedKey ->
                                                    if storedKey == fpCacheKey then
                                                        File.rawFile fpDataPath
                                                            |> BackendTask.toResult
                                                            |> BackendTask.andThen
                                                                (\dataResult ->
                                                                    case dataResult of
                                                                        Ok cached ->
                                                                            BackendTask.succeed cached

                                                                        Err _ ->
                                                                            evalProjectRulesSingle reviewProject fpIndices allModulesWithAst fpKeyPath fpDataPath fpCacheKey
                                                                )

                                                    else
                                                        evalProjectRulesSingle reviewProject fpIndices allModulesWithAst fpKeyPath fpDataPath fpCacheKey

                                                Err _ ->
                                                    evalProjectRulesSingle reviewProject fpIndices allModulesWithAst fpKeyPath fpDataPath fpCacheKey
                                        )
                                    |> BackendTask.map
                                        (\fpOutput ->
                                            [ moduleRuleOutput, importersResult.outputs, fpOutput ]
                                                |> List.filter (\s -> not (String.isEmpty (String.trim s)) && not (String.startsWith "ERROR:" s))
                                                |> String.join "\n"
                                        )

                            Err errStr ->
                                -- Eval error — fall back to non-intercepted eval
                                evalProjectRulesSingle reviewProject fpIndices allModulesWithAst fpKeyPath fpDataPath fpCacheKey
                                    |> BackendTask.map
                                        (\fpOutput ->
                                            [ moduleRuleOutput, importersResult.outputs, fpOutput ]
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

        -- Module rules: one Cache.compute per (rule, file) with NARROW key
        moduleRuleMonads =
            moduleRules
                |> List.concatMap
                    (\rule ->
                        let
                            profile =
                                profileForRule rule.name
                        in
                        modulesWithAst
                            |> List.indexedMap
                                (\fileIdx file ->
                                    let
                                        fileHashes =
                                            Dict.get file.path allFileAspectHashes
                                                |> Maybe.withDefault (SemanticHash.computeAspectHashesFromSource file.source)

                                        fileNarrowKey =
                                            narrowCacheKey profile file.path fileHashes allFileAspectHashes depGraph

                                        cacheKey =
                                            "mr|" ++ String.fromInt rule.index ++ "|" ++ helperHash ++ "|" ++ file.path ++ "|" ++ fileNarrowKey
                                    in
                                    Cache.do (Cache.writeFile cacheKey Cache.succeed) <| \keyHash ->
                                    Cache.compute [ "mr", String.fromInt rule.index, file.path ]
                                        keyHash
                                        (\() ->
                                            case
                                                InterpreterProject.prepareAndEval reviewProject
                                                    { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                                                    , expression = buildExpressionForRule rule.index [ file ]
                                                    , sourceOverrides = [ reviewRunnerHelperSource ]
                                                    }
                                            of
                                                Ok s -> s
                                                Err e -> e
                                        )
                                    <| \hash ->
                                    Cache.succeed
                                        { filename = Path.path ("mr-" ++ String.fromInt rule.index ++ "-" ++ String.fromInt fileIdx)
                                        , hash = hash
                                        }
                                )
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
                        moduleRules
                            |> List.concatMap
                                (\rule ->
                                    List.indexedMap
                                        (\fileIdx _ ->
                                            File.rawFile
                                                (Path.toString cacheResult.output ++ "/mr-" ++ String.fromInt rule.index ++ "-" ++ String.fromInt fileIdx)
                                                |> BackendTask.allowFatal
                                        )
                                        modulesWithAst
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


buildReviewIntercepts :
    Dict String Types.Value
    -> FastDict.Dict String Types.Intercept
buildReviewIntercepts preloadedCaches =
    FastDict.fromList
        [ ( "Review.Rule.initialCacheMarker"
          , Types.Intercept
                (\args _ _ ->
                    case args of
                        [ Types.String ruleName, _, defaultCache ] ->
                            case Dict.get ruleName preloadedCaches of
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
                        [ Types.String ruleName, _, cache ] ->
                            -- YIELD the cache to the framework for disk persistence.
                            Types.EvYield "review-cache-write"
                                (Types.Record
                                    (FastDict.fromList
                                        [ ( "ruleName", Types.String ruleName )
                                        , ( "cache", cache )
                                        ]
                                    )
                                )
                                (\_ ->
                                    -- After framework stores it, return the cache unchanged
                                    Types.EvOk cache
                                )

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


evalProjectRulesSingle :
    InterpreterProject
    -> List Int
    -> List { path : String, source : String, astJson : String }
    -> String
    -> String
    -> String
    -> BackendTask FatalError String
evalProjectRulesSingle reviewProject prIndices modulesWithAst cacheKeyPath cacheDataPath cacheKey =
    let
        result =
            InterpreterProject.prepareAndEval reviewProject
                { imports = [ "ReviewRunnerHelper", "ReviewConfig" ]
                , expression = buildExpressionForRules prIndices modulesWithAst
                , sourceOverrides = [ reviewRunnerHelperSource ]
                }

        output =
            case result of
                Ok s ->
                    s

                Err e ->
                    e
    in
    Do.do
        (Script.writeFile { path = cacheKeyPath, body = cacheKey } |> BackendTask.allowFatal)
    <| \_ ->
    Do.do
        (Script.writeFile { path = cacheDataPath, body = output } |> BackendTask.allowFatal)
    <| \_ ->
    BackendTask.succeed output


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


ensureReviewDeps : String -> BackendTask FatalError ()
ensureReviewDeps reviewDir =
    Script.exec "elm" [ "make", "src/ReviewConfig.elm", "--output", "/dev/null" ]
        |> BackendTask.inDir reviewDir
        |> BackendTask.toResult
        |> BackendTask.map (\_ -> ())


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
