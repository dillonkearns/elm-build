module InterpreterProject exposing (InterpreterProject, LoadProfile, benchmarkPackageSummaryCacheCodecs, eval, evalSimple, evalWith, evalWithCoverage, evalWithFileOverrides, evalWithSourceOverrides, getDepGraph, getPackageEnv, load, loadWith, loadWithProfile, precomputedValuesByModule, precomputedValuesCount, prepareAndEval, prepareAndEvalRaw, prepareAndEvalWithIntercepts, prepareAndEvalWithMemoizedFunctions, prepareAndEvalWithValues, prepareAndEvalWithValuesAndMemoizedFunctions, prepareAndEvalWithYield, prepareAndEvalWithYieldAndMemoizedFunctions, prepareAndEvalWithYieldState, prepareEvalSources)

{-| Evaluate and cache Elm expressions via the pure Elm interpreter.

Mirrors `ElmProject` structurally but replaces `elm make` + `node` with
`Cache.compute` + `Eval.Module.evalProject`.

-}

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Glob as Glob
import BackendTask.Time
import AstWireCodec
import Bytes
import Cache exposing (FileOrDirectory)
import Coverage
import DepGraph
import Dict exposing (Dict)
import Elm.Interface exposing (Exposed)
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Eval.Module
import Environment
import FastDict
import FatalError exposing (FatalError)
import FNV1a
import FunctionReachability
import Json.Decode as Decode
import Json.Encode
import Lamdera.Wire3
import MemoRuntime
import Path exposing (Path)
import ProjectRoots
import ValueWireCodec
import ProjectSources
import SemanticHash
import Set exposing (Set)
import Syntax exposing (fakeNode)
import Time
import Types


type alias ModuleGraph =
    { moduleToSource : Dict String String
    , moduleToFile : Dict String File
    , imports : Dict String (Set String)
    }


type alias CachedPackageModuleSummary =
    Eval.Module.CachedModuleSummary


type alias PackageEnvSeed =
    { interfaces : List ( ModuleName, List Exposed )
    , functionsByModule : List ( String, List FunctionImplementation )
    , importsByModule : List ( String, Types.ImportedNames )
    }


type alias LoadProfile =
    { resolveSourceDirectoriesMs : Int
    , loadPackageSourcesMs : Int
    , packageSummaryCacheHit : Int
    , packageSummaryCacheRoundtripOk : Int
    , packageSummaryCacheBytes : Int
    , loadPackageSummaryCacheMs : Int
    , decodePackageSummaryCacheMs : Int
    , validatePackageSummaryCacheMs : Int
    , writePackageSummaryCacheMs : Int
    , globUserSourcesMs : Int
    , readUserSourcesMs : Int
    , readExtraSourcesMs : Int
    , buildGraphMs : Int
    , parsePackageSourcesMs : Int
    , buildPackageSummariesFromParsedMs : Int
    , buildPackageEnvFromSummariesMs : Int
    , buildPackageEnvMs : Int
    , buildBaseUserEnvMs : Int
    , buildSemanticIndexMs : Int
    , cacheInputsMs : Int
    }


type alias Timed a =
    { value : a
    , ms : Int
    }


stageMs : Time.Posix -> Time.Posix -> Int
stageMs start finish =
    Time.posixToMillis finish - Time.posixToMillis start


withTiming : BackendTask FatalError a -> BackendTask FatalError (Timed a)
withTiming work =
    Do.do BackendTask.Time.now <| \start ->
    Do.do work <| \value ->
    Do.do BackendTask.Time.now <| \finish ->
    BackendTask.succeed
        { value = value
        , ms = stageMs start finish
        }


benchmarkThunk : (() -> a) -> BackendTask FatalError (Timed a)
benchmarkThunk thunk =
    Do.do BackendTask.Time.now <| \start ->
    let
        value =
            thunk ()
    in
    Do.do BackendTask.Time.now <| \finish ->
    BackendTask.succeed
        { value = value
        , ms = stageMs start finish
        }


readFilesTask : List String -> BackendTask FatalError (List String)
readFilesTask paths =
    BackendTask.Custom.run "readFiles"
        (Json.Encode.list Json.Encode.string paths)
        (Decode.list Decode.string)
        |> BackendTask.allowFatal


repeatBinaryEncode : Int -> List CachedPackageModuleSummary -> Int -> Int
repeatBinaryEncode iterations summaries acc =
    if iterations <= 0 then
        acc

    else
        repeatBinaryEncode
            (iterations - 1)
            summaries
            (acc + Bytes.width (encodePackageSummaryCache summaries))


repeatBinaryDecode : Int -> Bytes.Bytes -> Int -> Int
repeatBinaryDecode iterations bytes acc =
    if iterations <= 0 then
        acc

    else
        case decodePackageSummaryCache bytes of
            Just summaries ->
                repeatBinaryDecode (iterations - 1) bytes (acc + List.length summaries)

            Nothing ->
                -1


repeatEnvSeedBinaryEncode : Int -> PackageEnvSeed -> Int -> Int
repeatEnvSeedBinaryEncode iterations envSeed acc =
    if iterations <= 0 then
        acc

    else
        repeatEnvSeedBinaryEncode
            (iterations - 1)
            envSeed
            (acc + Bytes.width (encodePackageEnvSeed envSeed))


repeatEnvSeedBinaryDecode : Int -> Bytes.Bytes -> Int -> Int
repeatEnvSeedBinaryDecode iterations bytes acc =
    if iterations <= 0 then
        acc

    else
        case decodePackageEnvSeed bytes of
            Just envSeed ->
                repeatEnvSeedBinaryDecode
                    (iterations - 1)
                    bytes
                    (acc
                        + List.length envSeed.interfaces
                        + List.length envSeed.functionsByModule
                        + List.length envSeed.importsByModule
                    )

            Nothing ->
                -1


repeatShardedBinaryEncode : Int -> List CachedPackageModuleSummary -> Int -> Int
repeatShardedBinaryEncode iterations summaries acc =
    if iterations <= 0 then
        acc

    else
        let
            shardBytes =
                encodePackageSummaryCacheSharded summaries
        in
        repeatShardedBinaryEncode
            (iterations - 1)
            summaries
            (acc + List.foldl (\bytes total -> total + Bytes.width bytes) 0 shardBytes)


repeatShardedBinaryDecode : Int -> List Bytes.Bytes -> Int -> Int
repeatShardedBinaryDecode iterations shardBytes acc =
    if iterations <= 0 then
        acc

    else
        case decodePackageSummaryCacheSharded shardBytes of
            Just summaries ->
                repeatShardedBinaryDecode (iterations - 1) shardBytes (acc + List.length summaries)

            Nothing ->
                -1


repeatJsonEncode : Int -> List CachedPackageModuleSummary -> Int -> Int
repeatJsonEncode iterations summaries acc =
    if iterations <= 0 then
        acc

    else
        let
            jsonString =
                encodePackageSummaryCacheJson summaries
        in
        repeatJsonEncode (iterations - 1) summaries (acc + String.length jsonString)


repeatJsonDecode : Int -> String -> Int -> Int
repeatJsonDecode iterations jsonString acc =
    if iterations <= 0 then
        acc

    else
        case decodePackageSummaryCacheJson jsonString of
            Just summaries ->
                repeatJsonDecode (iterations - 1) jsonString (acc + List.length summaries)

            Nothing ->
                -1


packageSummaryCacheVersion : String
packageSummaryCacheVersion =
    "v7"


packageSummaryCacheBlobPath : String -> String -> String
packageSummaryCacheBlobPath cacheDir cacheKey =
    cacheDir ++ "/package-module-summaries-" ++ packageSummaryCacheVersion ++ "-" ++ cacheKey ++ ".blob"


packageSummaryCacheKey : List String -> String
packageSummaryCacheKey allPackageSources =
    allPackageSources
        |> List.map (FNV1a.hash >> String.fromInt)
        |> String.join "|"
        |> FNV1a.hash
        |> String.fromInt


{-| Cache version for the user-code normalization blob. Bump whenever
the `FunctionImplementation` wire format or the normalization rewrite shape
changes in a way that would make old blobs incorrect.
-}
userNormCacheVersion : String
userNormCacheVersion =
    "v4"


{-| Disk path for the combined user-normalization cache blob.

Key includes a combined hash over every user file's content and the package
environment fingerprint. Any user or package change invalidates the whole
blob and triggers a full re-normalize — coarser than per-file caching but
dramatically faster on the common "nothing changed" warm path (one binary
read + decode instead of dozens of stat/read round-trips).
-}
userNormCacheBlobPath : String -> String -> String -> String
userNormCacheBlobPath cacheDir packageKey userKey =
    cacheDir ++ "/user-normalized-" ++ userNormCacheVersion ++ "-" ++ packageKey ++ "-" ++ userKey ++ ".blob"


{-| Combined hash over every user file's source content, independent of
iteration order. Any byte change in any user file invalidates the blob.
-}
userNormBundleKey : List String -> String
userNormBundleKey userFileContents =
    userFileContents
        |> List.map (FNV1a.hash >> String.fromInt)
        |> List.sort
        |> String.join "|"
        |> FNV1a.hash
        |> String.fromInt


{-| Entry in the combined user-normalization blob: one per module, carrying
the module name, the list of normalized function implementations, and any
precomputed constant values (zero-arg functions evaluated to concrete Values
that survived `isLosslessValue`).
-}
type alias UserNormCacheEntry =
    { moduleName : List String
    , functions : List FunctionImplementation
    , precomputedValues : List ( String, Types.Value )
    }


encodeUserNormBundle : List UserNormCacheEntry -> Bytes.Bytes
encodeUserNormBundle entries =
    entries
        |> Lamdera.Wire3.encodeList encodeUserNormCacheEntry
        |> Lamdera.Wire3.bytesEncode


decodeUserNormBundle : Bytes.Bytes -> Maybe (List UserNormCacheEntry)
decodeUserNormBundle bytes =
    bytes
        |> Lamdera.Wire3.bytesDecode (Lamdera.Wire3.decodeList decodeUserNormCacheEntry)


encodeUserNormCacheEntry : UserNormCacheEntry -> Lamdera.Wire3.Encoder
encodeUserNormCacheEntry entry =
    -- Filter precomputed values to only those that round-trip through Wire3.
    -- `Int Infinity` (from `round (1 / 0)`), non-finite Floats, `JsonValue`,
    -- `PartiallyApplied`, etc. are held in the in-memory precomputed cache
    -- for runtime speedups but must not hit the on-disk blob — their encoded
    -- bytes don't decode back to the same value (or at all), which silently
    -- corrupts subsequent warm-run reads.
    let
        serializable : List ( String, Types.Value )
        serializable =
            List.filter
                (\( _, value ) -> Eval.Module.isLosslessValue value)
                entry.precomputedValues
    in
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ encodeModuleName entry.moduleName
        , Lamdera.Wire3.encodeList encodeFunctionImplementationNoRanges entry.functions
        , Lamdera.Wire3.encodeList
            (\( name, value ) ->
                Lamdera.Wire3.encodeSequenceWithoutLength
                    [ Lamdera.Wire3.encodeString name
                    , ValueWireCodec.encodeValue value
                    ]
            )
            serializable
        ]


decodeUserNormCacheEntry : Lamdera.Wire3.Decoder UserNormCacheEntry
decodeUserNormCacheEntry =
    Lamdera.Wire3.succeedDecode
        (\moduleName functions precomputedValues ->
            { moduleName = moduleName
            , functions = functions
            , precomputedValues = precomputedValues
            }
        )
        |> Lamdera.Wire3.andMapDecode decodeModuleName
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeFunctionImplementationNoRanges)
        |> Lamdera.Wire3.andMapDecode
            (Lamdera.Wire3.decodeList
                (Lamdera.Wire3.succeedDecode Tuple.pair
                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                    |> Lamdera.Wire3.andMapDecode ValueWireCodec.decodeValue
                )
            )


benchmarkPackageSummaryCacheCodecs :
    { projectDir : Path
    , skipPackages : Set String
    , patchSource : String -> String
    , patchUserSource : String -> String -> String
    , extraSourceFiles : List String
    , extraReachableImports : List String
    , sourceDirectories : Maybe (List String)
    , packageParseCacheDir : String
    , iterations : Int
    }
    -> BackendTask FatalError
        { iterations : Int
        , summaryCount : Int
        , binaryBytes : Int
        , metadataOnlyBinaryBytes : Int
        , topFunctionPayloadModules : List { moduleKey : String, functionCount : Int, functionBytes : Int }
        , envSeedBinaryBytes : Int
        , shardedBinaryBytes : Int
        , jsonChars : Int
        , seedCacheHit : Int
        , seedParsePackageSourcesMs : Int
        , seedBuildPackageSummariesFromParsedMs : Int
        , seedDecodePackageSummaryCacheMs : Int
        , binaryEncodeMs : Int
        , binaryDecodeMs : Int
        , metadataOnlyBinaryEncodeMs : Int
        , metadataOnlyBinaryDecodeMs : Int
        , envSeedBinaryEncodeMs : Int
        , envSeedBinaryDecodeMs : Int
        , shardedBinaryEncodeMs : Int
        , shardedBinaryDecodeMs : Int
        , jsonEncodeMs : Int
        , jsonDecodeMs : Int
        }
benchmarkPackageSummaryCacheCodecs config =
    Do.do
        (loadWithProfile
            { projectDir = config.projectDir
            , skipPackages = config.skipPackages
            , patchSource = config.patchSource
            , patchUserSource = \_ source -> source
            , extraSourceFiles = config.extraSourceFiles
            , extraReachableImports = config.extraReachableImports
            , sourceDirectories = config.sourceDirectories
            , normalizationRoots = Nothing
            , packageParseCacheDir = Just config.packageParseCacheDir
            }
        )
    <| \seedLoad ->
    Do.do
        (Glob.fromStringWithOptions
            (let
                options : Glob.Options
                options =
                    Glob.defaultOptions
             in
             { options | include = Glob.OnlyFiles }
            )
            (config.packageParseCacheDir ++ "/package-module-summaries-" ++ packageSummaryCacheVersion ++ "-*.blob")
        )
    <| \cacheCandidates ->
    case List.head (List.sort cacheCandidates) of
        Nothing ->
            BackendTask.fail (FatalError.fromString "Failed to find package summary cache blob for codec benchmark")

        Just cachePath ->
            Do.do (File.binaryFile cachePath |> BackendTask.allowFatal) <| \cacheBytes ->
                case decodePackageSummaryCache cacheBytes of
                    Nothing ->
                        BackendTask.fail (FatalError.fromString "Failed to decode package summary cache blob for codec benchmark")

                    Just summaries ->
                        let
                            metadataOnlySummaries =
                                summaries
                                    |> List.map (\summary -> { summary | functions = [] })

                            envSeed =
                                packageEnvSeedFromSummaries summaries

                            envSeedBytes =
                                encodePackageEnvSeed envSeed

                            metadataOnlyBytes =
                                encodePackageSummaryCache metadataOnlySummaries

                            topFunctionPayloadModules =
                                summaries
                                    |> List.map
                                    (\summary ->
                                        { moduleKey = Environment.moduleKey summary.moduleName
                                        , functionCount = List.length summary.functions
                                        , functionBytes =
                                            summary.functions
                                                |> Lamdera.Wire3.encodeList encodeFunctionImplementationNoRanges
                                                |> Lamdera.Wire3.bytesEncode
                                                |> Bytes.width
                                        }
                                    )
                                |> List.sortBy (.functionBytes >> negate)
                                |> List.take 12

                            jsonString =
                                encodePackageSummaryCacheJson summaries

                            shardedBytes =
                                encodePackageSummaryCacheSharded summaries
                        in
                        Do.do (benchmarkThunk (\_ -> repeatBinaryEncode config.iterations summaries 0)) <| \binaryEncodeTimed ->
                        Do.do (benchmarkThunk (\_ -> repeatBinaryDecode config.iterations cacheBytes 0)) <| \binaryDecodeTimed ->
                        Do.do (benchmarkThunk (\_ -> repeatBinaryEncode config.iterations metadataOnlySummaries 0)) <| \metadataOnlyBinaryEncodeTimed ->
                        Do.do (benchmarkThunk (\_ -> repeatBinaryDecode config.iterations metadataOnlyBytes 0)) <| \metadataOnlyBinaryDecodeTimed ->
                        Do.do (benchmarkThunk (\_ -> repeatEnvSeedBinaryEncode config.iterations envSeed 0)) <| \envSeedBinaryEncodeTimed ->
                        Do.do (benchmarkThunk (\_ -> repeatEnvSeedBinaryDecode config.iterations envSeedBytes 0)) <| \envSeedBinaryDecodeTimed ->
                        Do.do (benchmarkThunk (\_ -> repeatShardedBinaryEncode config.iterations summaries 0)) <| \shardedBinaryEncodeTimed ->
                        Do.do (benchmarkThunk (\_ -> repeatShardedBinaryDecode config.iterations shardedBytes 0)) <| \shardedBinaryDecodeTimed ->
                        Do.do (benchmarkThunk (\_ -> repeatJsonEncode config.iterations summaries 0)) <| \jsonEncodeTimed ->
                        Do.do (benchmarkThunk (\_ -> repeatJsonDecode config.iterations jsonString 0)) <| \jsonDecodeTimed ->
                        BackendTask.succeed
                            { iterations = config.iterations
                            , summaryCount = List.length summaries
                            , binaryBytes = Bytes.width cacheBytes
                            , metadataOnlyBinaryBytes = Bytes.width metadataOnlyBytes
                            , topFunctionPayloadModules = topFunctionPayloadModules
                        , envSeedBinaryBytes = Bytes.width envSeedBytes
                        , shardedBinaryBytes = List.foldl (\bytes total -> total + Bytes.width bytes) 0 shardedBytes
                        , jsonChars = String.length jsonString
                        , seedCacheHit = seedLoad.profile.packageSummaryCacheHit
                        , seedParsePackageSourcesMs = seedLoad.profile.parsePackageSourcesMs
                        , seedBuildPackageSummariesFromParsedMs = seedLoad.profile.buildPackageSummariesFromParsedMs
                        , seedDecodePackageSummaryCacheMs = seedLoad.profile.decodePackageSummaryCacheMs
                        , binaryEncodeMs = binaryEncodeTimed.ms
                        , binaryDecodeMs = binaryDecodeTimed.ms
                        , metadataOnlyBinaryEncodeMs = metadataOnlyBinaryEncodeTimed.ms
                        , metadataOnlyBinaryDecodeMs = metadataOnlyBinaryDecodeTimed.ms
                        , envSeedBinaryEncodeMs = envSeedBinaryEncodeTimed.ms
                        , envSeedBinaryDecodeMs = envSeedBinaryDecodeTimed.ms
                        , shardedBinaryEncodeMs = shardedBinaryEncodeTimed.ms
                        , shardedBinaryDecodeMs = shardedBinaryDecodeTimed.ms
                        , jsonEncodeMs = jsonEncodeTimed.ms
                        , jsonDecodeMs = jsonDecodeTimed.ms
                        }


encodePackageSummaryCache : List CachedPackageModuleSummary -> Bytes.Bytes
encodePackageSummaryCache summaries =
    summaries
        |> Lamdera.Wire3.encodeList encodeCachedPackageModuleSummary
        |> Lamdera.Wire3.bytesEncode


decodePackageSummaryCache : Bytes.Bytes -> Maybe (List CachedPackageModuleSummary)
decodePackageSummaryCache bytes =
    bytes
        |> Lamdera.Wire3.bytesDecode (Lamdera.Wire3.decodeList decodeCachedPackageModuleSummary)


encodePackageSummaryCacheSharded : List CachedPackageModuleSummary -> List Bytes.Bytes
encodePackageSummaryCacheSharded summaries =
    List.map
        (\summary ->
            summary
                |> encodeCachedPackageModuleSummary
                |> Lamdera.Wire3.bytesEncode
        )
        summaries


decodePackageSummaryCacheSharded : List Bytes.Bytes -> Maybe (List CachedPackageModuleSummary)
decodePackageSummaryCacheSharded shardBytes =
    shardBytes
        |> List.foldr
            (\bytes maybeSummaries ->
                case ( Lamdera.Wire3.bytesDecode decodeCachedPackageModuleSummary bytes, maybeSummaries ) of
                    ( Just summary, Just summaries ) ->
                        Just (summary :: summaries)

                    _ ->
                        Nothing
            )
            (Just [])


encodePackageSummaryCacheJson : List CachedPackageModuleSummary -> String
encodePackageSummaryCacheJson summaries =
    summaries
        |> Json.Encode.list encodeCachedPackageModuleSummaryJson
        |> Json.Encode.encode 0


decodePackageSummaryCacheJson : String -> Maybe (List CachedPackageModuleSummary)
decodePackageSummaryCacheJson jsonString =
    Decode.decodeString
        (Decode.list decodeCachedPackageModuleSummaryJson)
        jsonString
        |> Result.toMaybe


packageEnvSeedFromSummaries : List CachedPackageModuleSummary -> PackageEnvSeed
packageEnvSeedFromSummaries summaries =
    { interfaces =
        summaries
            |> List.map (\summary -> ( summary.moduleName, summary.interface ))
    , functionsByModule =
        summaries
            |> List.map
                (\summary ->
                    ( Environment.moduleKey summary.moduleName
                    , summary.functions
                    )
                )
    , importsByModule =
        summaries
            |> List.map
                (\summary ->
                    ( Environment.moduleKey summary.moduleName
                    , summary.importedNames
                    )
                )
    }


encodePackageEnvSeed : PackageEnvSeed -> Bytes.Bytes
encodePackageEnvSeed envSeed =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ Lamdera.Wire3.encodeList encodeInterfaceEntry envSeed.interfaces
        , Lamdera.Wire3.encodeList encodeFunctionModuleEntry envSeed.functionsByModule
        , Lamdera.Wire3.encodeList encodeImportModuleEntry envSeed.importsByModule
        ]
        |> Lamdera.Wire3.bytesEncode


decodePackageEnvSeed : Bytes.Bytes -> Maybe PackageEnvSeed
decodePackageEnvSeed bytes =
    bytes
        |> Lamdera.Wire3.bytesDecode
            (Lamdera.Wire3.succeedDecode PackageEnvSeed
                |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeInterfaceEntry)
                |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeFunctionModuleEntry)
                |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeImportModuleEntry)
            )


ensureDirTask : String -> BackendTask FatalError ()
ensureDirTask dirPath =
    BackendTask.Custom.run "ensureDir"
        (Json.Encode.string dirPath)
        (Decode.succeed ())
        |> BackendTask.allowFatal


{-| Normalize every user module in topological order with per-file caching.

Each module gets its own cache blob keyed on `(packageKey, moduleContentHash)`.
On a code change, only changed files are re-normalized; unchanged files load
from their individual cache. This makes the code-change scenario nearly as fast
as the fully-warm case.
-}
buildUserNormalizedEnv :
    { cacheDir : Maybe String
    , packageKey : String
    , userModules : List File
    , userFileContents : Dict String String
    }
    -> Eval.Module.ProjectEnv
    -> BackendTask FatalError Eval.Module.ProjectEnv
buildUserNormalizedEnv config envBeforeNorm =
    case config.cacheDir of
        Nothing ->
            BackendTask.succeed (normalizeAllUserModulesFresh config.userModules envBeforeNorm |> Tuple.first)

        Just cacheDir ->
            normalizePerFile cacheDir config.packageKey config.userModules config.userFileContents envBeforeNorm


normalizePerFile :
    String
    -> String
    -> List File
    -> Dict String String
    -> Eval.Module.ProjectEnv
    -> BackendTask FatalError Eval.Module.ProjectEnv
normalizePerFile cacheDir packageKey userModules userFileContents envBeforeNorm =
    Do.do (ensureDirTask cacheDir) <| \_ ->
    userModules
        |> List.foldl
            (\userFile envTask ->
                Do.do envTask <| \envAcc ->
                let
                    moduleName : List String
                    moduleName =
                        Eval.Module.fileModuleName userFile

                    moduleKey : String
                    moduleKey =
                        String.join "." moduleName

                    contentHash : String
                    contentHash =
                        Dict.get moduleKey userFileContents
                            |> Maybe.map (FNV1a.hash >> String.fromInt)
                            |> Maybe.withDefault "0"

                    perFilePath : String
                    perFilePath =
                        cacheDir
                            ++ "/user-norm-"
                            ++ userNormCacheVersion
                            ++ "-"
                            ++ packageKey
                            ++ "-"
                            ++ moduleKey
                            ++ "-"
                            ++ contentHash
                            ++ ".blob"
                in
                File.exists perFilePath
                    |> BackendTask.allowFatal
                    |> BackendTask.andThen
                        (\exists ->
                            if exists then
                                File.binaryFile perFilePath
                                    |> BackendTask.allowFatal
                                    |> BackendTask.map
                                        (\bytes ->
                                            case decodeUserNormBundle bytes of
                                                Just [ entry ] ->
                                                    applyUserNormBundle [ entry ] envAcc

                                                _ ->
                                                    Tuple.first (normalizeOneAndCache cacheDir perFilePath moduleName envAcc)
                                        )

                            else
                                let
                                    ( env, entry ) =
                                        normalizeOneAndCache cacheDir perFilePath moduleName envAcc
                                in
                                writeBinaryFile
                                    { path = perFilePath
                                    , bytes = encodeUserNormBundle [ entry ]
                                    }
                                    |> BackendTask.map (\_ -> env)
                        )
            )
            (BackendTask.succeed envBeforeNorm)


normalizeOneAndCache : String -> String -> List String -> Eval.Module.ProjectEnv -> ( Eval.Module.ProjectEnv, UserNormCacheEntry )
normalizeOneAndCache _ _ moduleName envAcc =
    let
        ( updatedEnv, normalizedFns ) =
            Eval.Module.normalizeOneModuleInEnv moduleName envAcc

        modulePrecomputed : List ( String, Types.Value )
        modulePrecomputed =
            Eval.Module.getModulePrecomputedValues moduleName updatedEnv
                |> FastDict.toList

        entry =
            { moduleName = moduleName
            , functions = FastDict.values normalizedFns
            , precomputedValues = modulePrecomputed
            }
    in
    ( updatedEnv, entry )


{-| Normalize every user module in topo order without consulting a cache.
Returns the updated env alongside the list of `UserNormCacheEntry` records
we can feed to the encoder.
-}
normalizeAllUserModulesFresh : List File -> Eval.Module.ProjectEnv -> ( Eval.Module.ProjectEnv, List UserNormCacheEntry )
normalizeAllUserModulesFresh userModules envBeforeNorm =
    userModules
        |> List.foldl
            (\userFile ( envAcc, entriesAcc ) ->
                let
                    moduleName : List String
                    moduleName =
                        Eval.Module.fileModuleName userFile

                    ( updatedEnv, normalizedFns ) =
                        Eval.Module.normalizeOneModuleInEnv moduleName envAcc

                    modulePrecomputed : List ( String, Types.Value )
                    modulePrecomputed =
                        Eval.Module.getModulePrecomputedValues moduleName updatedEnv
                            |> FastDict.toList
                in
                ( updatedEnv
                , { moduleName = moduleName
                  , functions = FastDict.values normalizedFns
                  , precomputedValues = modulePrecomputed
                  }
                    :: entriesAcc
                )
            )
            ( envBeforeNorm, [] )
        |> Tuple.mapSecond List.reverse


{-| Install a decoded `UserNormCacheEntry` list into the env. Each entry
contains the functions whose bodies were rewritten during normalization (overlaid
onto the originals) AND the precomputed constant values.
-}
applyUserNormBundle : List UserNormCacheEntry -> Eval.Module.ProjectEnv -> Eval.Module.ProjectEnv
applyUserNormBundle entries env =
    List.foldl
        (\entry currentEnv ->
            let
                deltaDict : FastDict.Dict String FunctionImplementation
                deltaDict =
                    entry.functions
                        |> List.map (\f -> ( Node.value f.name, f ))
                        |> FastDict.fromList

                precomputedDict : FastDict.Dict String Types.Value
                precomputedDict =
                    FastDict.fromList entry.precomputedValues
            in
            currentEnv
                |> Eval.Module.mergeModuleFunctionsIntoEnv entry.moduleName deltaDict
                |> Eval.Module.setModulePrecomputedValues entry.moduleName precomputedDict
        )
        env
        entries


writeBinaryFile : { path : String, bytes : Bytes.Bytes } -> BackendTask FatalError ()
writeBinaryFile { path, bytes } =
    BackendTask.Custom.run "writeBinaryFile"
        (Json.Encode.object
            [ ( "path", Json.Encode.string path )
            , ( "bytes"
              , bytes
                    |> Lamdera.Wire3.intListFromBytes
                    |> Json.Encode.list Json.Encode.int
              )
            ]
        )
        (Decode.succeed ())
        |> BackendTask.allowFatal


encodeCachedPackageModuleSummary : CachedPackageModuleSummary -> Lamdera.Wire3.Encoder
encodeCachedPackageModuleSummary summary =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ encodeModuleName summary.moduleName
        , Lamdera.Wire3.encodeList encodeExposed summary.interface
        , encodeImportedNames summary.importedNames
        , Lamdera.Wire3.encodeList encodeFunctionImplementationNoRanges summary.functions
        ]


decodeCachedPackageModuleSummary : Lamdera.Wire3.Decoder CachedPackageModuleSummary
decodeCachedPackageModuleSummary =
    Lamdera.Wire3.succeedDecode
        (\moduleName interface importedNames functions ->
            { moduleName = moduleName
            , interface = interface
            , importedNames = importedNames
            , functions = functions
            }
        )
        |> Lamdera.Wire3.andMapDecode decodeModuleName
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeExposed)
        |> Lamdera.Wire3.andMapDecode decodeImportedNames
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeFunctionImplementationNoRanges)


encodeCachedPackageModuleSummaryJson : CachedPackageModuleSummary -> Json.Encode.Value
encodeCachedPackageModuleSummaryJson summary =
    Json.Encode.object
        [ ( "moduleName", Json.Encode.list Json.Encode.string summary.moduleName )
        , ( "interface", Json.Encode.list encodeExposedJson summary.interface )
        , ( "importedNames", encodeImportedNamesJson summary.importedNames )
        , ( "functions", Json.Encode.list encodeFunctionImplementationNoRangesJson summary.functions )
        ]


decodeCachedPackageModuleSummaryJson : Decode.Decoder CachedPackageModuleSummary
decodeCachedPackageModuleSummaryJson =
    Decode.map4
        (\moduleName interface importedNames functions ->
            { moduleName = moduleName
            , interface = interface
            , importedNames = importedNames
            , functions = functions
            }
        )
        (Decode.field "moduleName" (Decode.list Decode.string))
        (Decode.field "interface" (Decode.list decodeExposedJson))
        (Decode.field "importedNames" decodeImportedNamesJson)
        (Decode.field "functions" (Decode.list decodeFunctionImplementationNoRangesJson))


encodeModuleName : ModuleName -> Lamdera.Wire3.Encoder
encodeModuleName moduleName =
    Lamdera.Wire3.encodeList Lamdera.Wire3.encodeString moduleName


decodeModuleName : Lamdera.Wire3.Decoder ModuleName
decodeModuleName =
    Lamdera.Wire3.decodeList Lamdera.Wire3.decodeString


encodeExposed : Exposed -> Lamdera.Wire3.Encoder
encodeExposed exposed =
    case exposed of
        Elm.Interface.Function name ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeInt 0
                , Lamdera.Wire3.encodeString name
                ]

        Elm.Interface.CustomType ( name, constructors ) ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeInt 1
                , Lamdera.Wire3.encodeString name
                , Lamdera.Wire3.encodeList Lamdera.Wire3.encodeString constructors
                ]

        Elm.Interface.Alias name ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeInt 2
                , Lamdera.Wire3.encodeString name
                ]

        Elm.Interface.Operator infix_ ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeInt 3
                , encodeInfixDirectionRaw (Node.value infix_.direction)
                , Lamdera.Wire3.encodeInt (Node.value infix_.precedence)
                , Lamdera.Wire3.encodeString (Node.value infix_.operator)
                , Lamdera.Wire3.encodeString (Node.value infix_.function)
                ]


decodeExposed : Lamdera.Wire3.Decoder Exposed
decodeExposed =
    Lamdera.Wire3.decodeInt
        |> Lamdera.Wire3.andThenDecode
            (\tag ->
                case tag of
                    0 ->
                        Lamdera.Wire3.succeedDecode Elm.Interface.Function
                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString

                    1 ->
                        Lamdera.Wire3.succeedDecode (\name constructors -> Elm.Interface.CustomType ( name, constructors ))
                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                            |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList Lamdera.Wire3.decodeString)

                    2 ->
                        Lamdera.Wire3.succeedDecode Elm.Interface.Alias
                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString

                    _ ->
                        Lamdera.Wire3.succeedDecode
                            (\direction precedence operator_ function_ ->
                                Elm.Interface.Operator
                                    { direction = fakeNode direction
                                    , precedence = fakeNode precedence
                                    , operator = fakeNode operator_
                                    , function = fakeNode function_
                                    }
                            )
                            |> Lamdera.Wire3.andMapDecode decodeInfixDirectionRaw
                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt
                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
            )


encodeExposedJson : Exposed -> Json.Encode.Value
encodeExposedJson exposed =
    case exposed of
        Elm.Interface.Function name ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "function" )
                , ( "name", Json.Encode.string name )
                ]

        Elm.Interface.CustomType ( name, constructors ) ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "custom-type" )
                , ( "name", Json.Encode.string name )
                , ( "constructors", Json.Encode.list Json.Encode.string constructors )
                ]

        Elm.Interface.Alias name ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "alias" )
                , ( "name", Json.Encode.string name )
                ]

        Elm.Interface.Operator infix_ ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "operator" )
                , ( "infix", Infix.encode infix_ )
                ]


decodeExposedJson : Decode.Decoder Exposed
decodeExposedJson =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "function" ->
                        Decode.map Elm.Interface.Function
                            (Decode.field "name" Decode.string)

                    "custom-type" ->
                        Decode.map2 (\name constructors -> Elm.Interface.CustomType ( name, constructors ))
                            (Decode.field "name" Decode.string)
                            (Decode.field "constructors" (Decode.list Decode.string))

                    "alias" ->
                        Decode.map Elm.Interface.Alias
                            (Decode.field "name" Decode.string)

                    "operator" ->
                        Decode.map Elm.Interface.Operator
                            (Decode.field "infix" Infix.decoder)

                    _ ->
                        Decode.fail ("Unknown exposed tag: " ++ tag)
            )


encodeImportedNames : Types.ImportedNames -> Lamdera.Wire3.Encoder
encodeImportedNames importedNames =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ encodeQualifiedMapping importedNames.aliases
        , encodeQualifiedMapping importedNames.exposedValues
        , encodeQualifiedMapping importedNames.exposedConstructors
        ]


decodeImportedNames : Lamdera.Wire3.Decoder Types.ImportedNames
decodeImportedNames =
    Lamdera.Wire3.succeedDecode
        (\aliases exposedValues exposedConstructors ->
            { aliases = aliases
            , exposedValues = exposedValues
            , exposedConstructors = exposedConstructors
            }
        )
        |> Lamdera.Wire3.andMapDecode decodeQualifiedMapping
        |> Lamdera.Wire3.andMapDecode decodeQualifiedMapping
        |> Lamdera.Wire3.andMapDecode decodeQualifiedMapping


encodeImportedNamesJson : Types.ImportedNames -> Json.Encode.Value
encodeImportedNamesJson importedNames =
    Json.Encode.object
        [ ( "aliases", encodeQualifiedMappingJson importedNames.aliases )
        , ( "exposedValues", encodeQualifiedMappingJson importedNames.exposedValues )
        , ( "exposedConstructors", encodeQualifiedMappingJson importedNames.exposedConstructors )
        ]


decodeImportedNamesJson : Decode.Decoder Types.ImportedNames
decodeImportedNamesJson =
    Decode.map3
        (\aliases exposedValues exposedConstructors ->
            { aliases = aliases
            , exposedValues = exposedValues
            , exposedConstructors = exposedConstructors
            }
        )
        (Decode.field "aliases" decodeQualifiedMappingJson)
        (Decode.field "exposedValues" decodeQualifiedMappingJson)
        (Decode.field "exposedConstructors" decodeQualifiedMappingJson)


encodeQualifiedMapping : FastDict.Dict String ( ModuleName, String ) -> Lamdera.Wire3.Encoder
encodeQualifiedMapping mapping =
    mapping
        |> FastDict.toList
        |> Lamdera.Wire3.encodeList
            (\( name, ( moduleName, _ ) ) ->
                Lamdera.Wire3.encodeSequenceWithoutLength
                    [ Lamdera.Wire3.encodeString name
                    , encodeModuleName moduleName
                    ]
            )


decodeQualifiedMapping : Lamdera.Wire3.Decoder (FastDict.Dict String ( ModuleName, String ))
decodeQualifiedMapping =
    Lamdera.Wire3.succeedDecode FastDict.fromList
        |> Lamdera.Wire3.andMapDecode
            (Lamdera.Wire3.decodeList
                (Lamdera.Wire3.succeedDecode
                    (\name moduleName ->
                        ( name, ( moduleName, Environment.moduleKey moduleName ) )
                    )
                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                    |> Lamdera.Wire3.andMapDecode decodeModuleName
                )
            )


encodeQualifiedMappingJson : FastDict.Dict String ( ModuleName, String ) -> Json.Encode.Value
encodeQualifiedMappingJson mapping =
    mapping
        |> FastDict.toList
        |> Json.Encode.list
            (\( name, ( moduleName, _ ) ) ->
                Json.Encode.object
                    [ ( "name", Json.Encode.string name )
                    , ( "moduleName", Json.Encode.list Json.Encode.string moduleName )
                    ]
            )


decodeQualifiedMappingJson : Decode.Decoder (FastDict.Dict String ( ModuleName, String ))
decodeQualifiedMappingJson =
    Decode.map FastDict.fromList
        (Decode.list
            (Decode.map2
                (\name moduleName ->
                    ( name, ( moduleName, Environment.moduleKey moduleName ) )
                )
                (Decode.field "name" Decode.string)
                (Decode.field "moduleName" (Decode.list Decode.string))
            )
        )


encodeInterfaceEntry : ( ModuleName, List Exposed ) -> Lamdera.Wire3.Encoder
encodeInterfaceEntry ( moduleName, interface ) =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ encodeModuleName moduleName
        , Lamdera.Wire3.encodeList encodeExposed interface
        ]


decodeInterfaceEntry : Lamdera.Wire3.Decoder ( ModuleName, List Exposed )
decodeInterfaceEntry =
    Lamdera.Wire3.succeedDecode Tuple.pair
        |> Lamdera.Wire3.andMapDecode decodeModuleName
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeExposed)


encodeFunctionModuleEntry : ( String, List FunctionImplementation ) -> Lamdera.Wire3.Encoder
encodeFunctionModuleEntry ( moduleKey, functions ) =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ Lamdera.Wire3.encodeString moduleKey
        , Lamdera.Wire3.encodeList encodeFunctionImplementationNoRanges functions
        ]


decodeFunctionModuleEntry : Lamdera.Wire3.Decoder ( String, List FunctionImplementation )
decodeFunctionModuleEntry =
    Lamdera.Wire3.succeedDecode Tuple.pair
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeFunctionImplementationNoRanges)


encodeImportModuleEntry : ( String, Types.ImportedNames ) -> Lamdera.Wire3.Encoder
encodeImportModuleEntry ( moduleKey, importedNames ) =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ Lamdera.Wire3.encodeString moduleKey
        , encodeImportedNames importedNames
        ]


decodeImportModuleEntry : Lamdera.Wire3.Decoder ( String, Types.ImportedNames )
decodeImportModuleEntry =
    Lamdera.Wire3.succeedDecode Tuple.pair
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
        |> Lamdera.Wire3.andMapDecode decodeImportedNames


encodeFunctionImplementationNoRanges : FunctionImplementation -> Lamdera.Wire3.Encoder
encodeFunctionImplementationNoRanges implementation =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ Lamdera.Wire3.encodeString (Node.value implementation.name)
        , Lamdera.Wire3.encodeList (AstWireCodec.encodePattern << Node.value) implementation.arguments
        , AstWireCodec.encodeExpression (Node.value implementation.expression)
        ]


decodeFunctionImplementationNoRanges : Lamdera.Wire3.Decoder FunctionImplementation
decodeFunctionImplementationNoRanges =
    Lamdera.Wire3.succeedDecode
        (\name arguments expression ->
            { name = fakeNode name
            , arguments = List.map fakeNode arguments
            , expression = fakeNode expression
            }
        )
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList AstWireCodec.decodePattern)
        |> Lamdera.Wire3.andMapDecode AstWireCodec.decodeExpression


encodeFunctionImplementationNoRangesJson : FunctionImplementation -> Json.Encode.Value
encodeFunctionImplementationNoRangesJson implementation =
    Elm.Syntax.Expression.encodeFunction
        { documentation = Nothing
        , signature = Nothing
        , declaration = fakeNode implementation
        }


decodeFunctionImplementationNoRangesJson : Decode.Decoder FunctionImplementation
decodeFunctionImplementationNoRangesJson =
    Elm.Syntax.Expression.functionDecoder
        |> Decode.map (\function_ -> Node.value function_.declaration)


encodeInfixDirectionRaw : InfixDirection -> Lamdera.Wire3.Encoder
encodeInfixDirectionRaw direction =
    case direction of
        Left ->
            Lamdera.Wire3.encodeInt 0

        Right ->
            Lamdera.Wire3.encodeInt 1

        Non ->
            Lamdera.Wire3.encodeInt 2


decodeInfixDirectionRaw : Lamdera.Wire3.Decoder InfixDirection
decodeInfixDirectionRaw =
    Lamdera.Wire3.decodeInt
        |> Lamdera.Wire3.andThenDecode
            (\tag ->
                case tag of
                    0 ->
                        Lamdera.Wire3.succeedDecode Left

                    1 ->
                        Lamdera.Wire3.succeedDecode Right

                    _ ->
                        Lamdera.Wire3.succeedDecode Non
            )


type InterpreterProject
    = InterpreterProject
        { sourceDirectories : List String
        , inputsByPath : Dict String ( Path, Cache.Monad FileOrDirectory )
        , userFileContents : Dict String String
        , depGraph : DepGraph.Graph
        , patchedPackageSources : List String
        , extraSources : List String
        , moduleGraph : ModuleGraph
        , packageModuleNames : Set String
        , packageEnv : Eval.Module.ProjectEnv
        , baseUserEnv : Maybe Eval.Module.ProjectEnv
        , semanticIndex : SemanticHash.DeclarationIndex
        }


{-| Initialize an InterpreterProject with default settings.

Reads elm.json for source directories, loads all package dependencies,
and sets up the project for interpreter evaluation.

-}
load :
    { projectDir : Path }
    -> BackendTask FatalError InterpreterProject
load { projectDir } =
    loadWith
        { projectDir = projectDir
        , skipPackages = Set.empty
        , patchSource = identity
        , patchUserSource = \_ source -> source
        , extraSourceFiles = []
        , extraReachableImports = []
        , sourceDirectories = Nothing
        , normalizationRoots = Nothing
        }


{-| Initialize an InterpreterProject with advanced options.

  - `skipPackages` — package names to exclude (e.g. those with kernel code)
  - `patchSource` — transform applied to each package source after loading
  - `extraSourceFiles` — additional source files to include
  - `sourceDirectories` — `Nothing` reads from elm.json, `Just` overrides
  - `normalizationRoots` — list of user module names whose top-level
    functions are the evaluation entry points. When `Just`, the loader
    runs a function-level reachability walk (see
    `FunctionReachability.computeReachable`) from those modules and only
    normalizes user modules containing a reachable function. Modules
    outside the live set stay parsed-but-unnormalized: they're still
    loaded into `ProjectEnv` so runtime lookups work, they just skip
    the expensive eager `tryNormalizeConstant` pass on cold-user load.

    Cache interaction: the `user-norm-v4-*.blob` cache key is the
    per-module content hash, independent of the reachability set. Cache
    hits always apply; `normalizationRoots` only gates whether a cache
    MISS does work. Over time the on-disk cache fills up regardless of
    which plan any individual run uses.

    `Nothing` means "no plan information, normalize every user module
    eagerly" — the safe conservative default for raw loaders,
    interactive REPL evaluation, and anything where the eventual
    evaluation plan isn't known at load time.

    Callers that DO know their plan upfront should pass it: test runners
    pass test module names, ReviewRunner passes `["ReviewConfig"]`,
    benchmark runners pass their fixture modules. See
    `src/FunctionReachability.elm` for the walk semantics.

-}
loadWith :
    { projectDir : Path
    , skipPackages : Set String
    , patchSource : String -> String
    , patchUserSource : String -> String -> String
    , extraSourceFiles : List String
    , extraReachableImports : List String
    , sourceDirectories : Maybe (List String)
    , normalizationRoots : Maybe (List String)
    }
    -> BackendTask FatalError InterpreterProject
loadWith config =
    loadWithProfile
        { projectDir = config.projectDir
        , skipPackages = config.skipPackages
        , patchSource = config.patchSource
        , patchUserSource = config.patchUserSource
        , extraSourceFiles = config.extraSourceFiles
        , extraReachableImports = config.extraReachableImports
        , sourceDirectories = config.sourceDirectories
        , normalizationRoots = config.normalizationRoots
        , packageParseCacheDir =
            -- Default to the project's `.elm-build` directory so the package
            -- summary cache (including normalized top-level constants) is
            -- persisted across runs without every caller having to opt in.
            Just (Path.toString config.projectDir ++ "/.elm-build")
        }
        |> BackendTask.map .project


loadWithProfile :
    { projectDir : Path
    , skipPackages : Set String
    , patchSource : String -> String
    , patchUserSource : String -> String -> String
    , extraSourceFiles : List String
    , extraReachableImports : List String
    , sourceDirectories : Maybe (List String)
    , normalizationRoots : Maybe (List String)
    , packageParseCacheDir : Maybe String
    }
    -> BackendTask FatalError { project : InterpreterProject, profile : LoadProfile }
loadWithProfile config =
    Do.do
        (withTiming
            (case config.sourceDirectories of
                Just dirs ->
                    BackendTask.succeed dirs

                Nothing ->
                    readSourceDirectories config.projectDir
            )
        )
    <| \sourceDirectoriesTimed ->
    let
        sourceDirectories =
            sourceDirectoriesTimed.value
    in
    Do.do
        (withTiming
            (ProjectSources.loadPackageDepsCached
                { projectDir = config.projectDir
                , skipPackages = config.skipPackages
                }
            )
        )
    <| \packageSourcesTimed ->
    let
        patchedPackageSources : List String
        patchedPackageSources =
            List.map config.patchSource packageSourcesTimed.value

        userSourceGlobs : List String
        userSourceGlobs =
            sourceDirectories
                |> List.map (\dir -> dir ++ "/**/*.elm")
    in
    Do.do
        (withTiming
            (userSourceGlobs
                |> List.map
                    (\globPattern ->
                        Glob.fromStringWithOptions
                            (let
                                o : Glob.Options
                                o =
                                    Glob.defaultOptions
                             in
                             { o | include = Glob.OnlyFiles }
                            )
                            globPattern
                    )
                |> BackendTask.Extra.combine
                |> BackendTask.map (List.concat >> List.sort)
            )
        )
    <| \elmFilesTimed ->
    let
        elmFiles =
            elmFilesTimed.value
    in
    Do.do
        (withTiming
            (readFilesTask elmFiles
                |> BackendTask.map (List.map2 Tuple.pair elmFiles)
            )
        )
    <| \userFileContentsTimed ->
    let
        userFileContents =
            userFileContentsTimed.value
                |> List.map
                    (\( filePath, content ) ->
                        ( filePath
                        , config.patchUserSource filePath content
                        )
                    )
    in
    Do.do
        (withTiming
            (readFilesTask config.extraSourceFiles
                |> BackendTask.map (List.map2 Tuple.pair config.extraSourceFiles)
            )
        )
    <| \extraFileContentsTimed ->
    let
        extraFileContents =
            extraFileContentsTimed.value
    in
    Do.do BackendTask.Time.now <| \graphStart ->
    let
        depGraph : DepGraph.Graph
        depGraph =
            DepGraph.buildGraph
                { sourceDirectories = sourceDirectories
                , files =
                    userFileContents
                        |> List.map
                            (\( filePath, content ) ->
                                { filePath = filePath
                                , content = content
                                }
                            )
                }

        allUserPaths : List Path
        allUserPaths =
            elmFiles
                |> List.map Path.path

        allSourceStrings : List String
        allSourceStrings =
            patchedPackageSources
                ++ List.map Tuple.second extraFileContents
                ++ List.map Tuple.second userFileContents

        allModules : List ( String, String )
        allModules =
            List.filterMap
                (\src ->
                    DepGraph.parseModuleName src
                        |> Maybe.map (\name -> ( name, src ))
                )
                allSourceStrings

        userParsedFiles : Dict String File
        userParsedFiles =
            userFileContents
                |> List.filterMap
                    (\( _, content ) ->
                        case Elm.Parser.parseToFile content of
                            Ok file ->
                                DepGraph.parseModuleName content
                                    |> Maybe.map (\name -> ( name, file ))

                            Err _ ->
                                Nothing
                    )
                |> Dict.fromList

        moduleGraph : ModuleGraph
        moduleGraph =
            { moduleToSource = Dict.fromList allModules
            , moduleToFile = userParsedFiles
            , imports =
                allModules
                    |> List.map
                        (\( name, src ) ->
                            ( name, DepGraph.parseImports src |> Set.fromList )
                        )
                    |> Dict.fromList
            }

        allStableSources : List String
        allStableSources =
            patchedPackageSources ++ List.map Tuple.second extraFileContents

        pkgModuleNames : Set String
        pkgModuleNames =
            allStableSources
                |> List.filterMap DepGraph.parseModuleName
                |> Set.fromList

        rootModuleNames : Set String
        rootModuleNames =
            ProjectRoots.computeRootModuleNames
                { allModuleNames = moduleGraph.moduleToSource |> Dict.keys |> Set.fromList
                , pkgModuleNames = pkgModuleNames
                , extraReachableImports = config.extraReachableImports
                }

        reachablePackageModuleNames : Set String
        reachablePackageModuleNames =
            reachableModules moduleGraph rootModuleNames
                |> Set.intersect pkgModuleNames

        allPackageSources : List String
        allPackageSources =
            topoSortModules moduleGraph reachablePackageModuleNames
    in
    Do.do BackendTask.Time.now <| \graphFinish ->
    let
        buildGraphMs =
            stageMs graphStart graphFinish

        packageSummaryCachePath : Maybe String
        packageSummaryCachePath =
            config.packageParseCacheDir
                |> Maybe.map (\cacheDir -> packageSummaryCacheBlobPath cacheDir (packageSummaryCacheKey allPackageSources))

        parseAndSeedPackageSummaries :
            { packageSummaryCacheHit : Int
            , loadPackageSummaryCacheMs : Int
            , decodePackageSummaryCacheMs : Int
            }
            -> BackendTask FatalError
                { packageSummaries : List CachedPackageModuleSummary
                , packageSummaryCacheHit : Int
                , packageSummaryCacheRoundtripOk : Int
                , packageSummaryCacheBytes : Int
                , loadPackageSummaryCacheMs : Int
                , decodePackageSummaryCacheMs : Int
                , validatePackageSummaryCacheMs : Int
                , parsePackageSourcesMs : Int
                , buildPackageSummariesFromParsedMs : Int
                , writePackageSummaryCacheMs : Int
                }
        parseAndSeedPackageSummaries cacheMetrics =
            Do.do BackendTask.Time.now <| \parsePackageSourcesStart ->
            let
                parsedPackageSourcesResult =
                    Eval.Module.parseProjectSources allPackageSources
            in
            Do.do BackendTask.Time.now <| \parsePackageSourcesFinish ->
            let
                parsePackageSourcesMs =
                    stageMs parsePackageSourcesStart parsePackageSourcesFinish
            in
            case parsedPackageSourcesResult of
                Err _ ->
                    BackendTask.fail (FatalError.fromString "Failed to build package environment")

                Ok parsedPackageSources ->
                    Do.do BackendTask.Time.now <| \buildPackageSummariesStart ->
                    let
                        packageSummaries =
                            -- Normalize top-level constants here so the cache
                            -- stores the rewritten bodies. Subsequent project
                            -- loads hit the summary cache and skip both the
                            -- normalization pass AND its re-evaluation cost.
                            Eval.Module.normalizeSummaries
                                (Eval.Module.buildCachedModuleSummariesFromParsed parsedPackageSources)
                    in
                    Do.do BackendTask.Time.now <| \buildPackageSummariesFinish ->
                    let
                        buildPackageSummariesFromParsedMs =
                            stageMs buildPackageSummariesStart buildPackageSummariesFinish
                    in
                    case packageSummaryCachePath of
                        Just cachePath ->
                            let
                                cacheBytes =
                                    encodePackageSummaryCache packageSummaries
                            in
                            Do.do BackendTask.Time.now <| \validateCacheStart ->
                            let
                                cacheRoundtripOk =
                                    decodePackageSummaryCache cacheBytes /= Nothing
                            in
                            Do.do BackendTask.Time.now <| \validateCacheFinish ->
                            let
                                baseInfo =
                                    { packageSummaries = packageSummaries
                                    , packageSummaryCacheHit = cacheMetrics.packageSummaryCacheHit
                                    , packageSummaryCacheRoundtripOk =
                                        if cacheRoundtripOk then
                                            1

                                        else
                                            0
                                    , packageSummaryCacheBytes = Bytes.width cacheBytes
                                    , loadPackageSummaryCacheMs = cacheMetrics.loadPackageSummaryCacheMs
                                    , decodePackageSummaryCacheMs = cacheMetrics.decodePackageSummaryCacheMs
                                    , validatePackageSummaryCacheMs = stageMs validateCacheStart validateCacheFinish
                                    , parsePackageSourcesMs = parsePackageSourcesMs
                                    , buildPackageSummariesFromParsedMs = buildPackageSummariesFromParsedMs
                                    , writePackageSummaryCacheMs = 0
                                    }
                            in
                            if cacheRoundtripOk then
                                Do.do
                                    (withTiming
                                        (writeBinaryFile
                                            { path = cachePath
                                            , bytes = cacheBytes
                                            }
                                        )
                                    )
                                <| \writeCacheTimed ->
                                BackendTask.succeed
                                    { baseInfo | writePackageSummaryCacheMs = writeCacheTimed.ms }

                            else
                                BackendTask.succeed baseInfo

                        Nothing ->
                            BackendTask.succeed
                                { packageSummaries = packageSummaries
                                , packageSummaryCacheHit = cacheMetrics.packageSummaryCacheHit
                                , packageSummaryCacheRoundtripOk = 0
                                , packageSummaryCacheBytes = 0
                                , loadPackageSummaryCacheMs = cacheMetrics.loadPackageSummaryCacheMs
                                , decodePackageSummaryCacheMs = cacheMetrics.decodePackageSummaryCacheMs
                                , validatePackageSummaryCacheMs = 0
                                , parsePackageSourcesMs = parsePackageSourcesMs
                                , buildPackageSummariesFromParsedMs = buildPackageSummariesFromParsedMs
                                , writePackageSummaryCacheMs = 0
                                }
    in
    Do.do
        (case packageSummaryCachePath of
            Just cachePath ->
                Do.do (File.exists cachePath |> BackendTask.allowFatal) <| \cacheExists ->
                if cacheExists then
                    Do.do (withTiming (File.binaryFile cachePath |> BackendTask.allowFatal)) <| \readCacheTimed ->
                    Do.do BackendTask.Time.now <| \decodeCacheStart ->
                    let
                        decodedCache =
                            decodePackageSummaryCache readCacheTimed.value
                    in
                    Do.do BackendTask.Time.now <| \decodeCacheFinish ->
                    case decodedCache of
                        Just packageSummaries ->
                            BackendTask.succeed
                                { packageSummaries = packageSummaries
                                , packageSummaryCacheHit = 1
                                , packageSummaryCacheRoundtripOk = 1
                                , packageSummaryCacheBytes = Bytes.width readCacheTimed.value
                                , loadPackageSummaryCacheMs = readCacheTimed.ms
                                , decodePackageSummaryCacheMs = stageMs decodeCacheStart decodeCacheFinish
                                , validatePackageSummaryCacheMs = 0
                                , parsePackageSourcesMs = 0
                                , buildPackageSummariesFromParsedMs = 0
                                , writePackageSummaryCacheMs = 0
                                }

                        Nothing ->
                            parseAndSeedPackageSummaries
                                { packageSummaryCacheHit = 0
                                , loadPackageSummaryCacheMs = readCacheTimed.ms
                                , decodePackageSummaryCacheMs = stageMs decodeCacheStart decodeCacheFinish
                                }

                else
                    parseAndSeedPackageSummaries
                        { packageSummaryCacheHit = 0
                        , loadPackageSummaryCacheMs = 0
                        , decodePackageSummaryCacheMs = 0
                        }

            Nothing ->
                parseAndSeedPackageSummaries
                    { packageSummaryCacheHit = 0
                    , loadPackageSummaryCacheMs = 0
                    , decodePackageSummaryCacheMs = 0
                    }
        )
    <| \packageSummariesInfo ->
    let
        packageSummaries =
            packageSummariesInfo.packageSummaries
    in
            Do.do BackendTask.Time.now <| \buildPackageEnvStart ->
            let
                pkgEnvResult : Result Types.Error Eval.Module.ProjectEnv
                pkgEnvResult =
                    Eval.Module.buildProjectEnvFromSummaries packageSummaries
            in
            Do.do BackendTask.Time.now <| \buildPackageEnvFinish ->
            let
                buildPackageEnvFromSummariesMs =
                    stageMs buildPackageEnvStart buildPackageEnvFinish

                buildPackageEnvMs =
                    packageSummariesInfo.parsePackageSourcesMs
                        + packageSummariesInfo.buildPackageSummariesFromParsedMs
                        + buildPackageEnvFromSummariesMs
            in
            case pkgEnvResult of
                Err _ ->
                    BackendTask.fail (FatalError.fromString "Failed to build package environment")

                Ok pkgEnv ->
                    Do.do BackendTask.Time.now <| \baseUserEnvStart ->
                    let
                        userModuleNamesSet : Set String
                        userModuleNamesSet =
                            userParsedFiles |> Dict.keys |> Set.fromList

                        userModulesInOrder : List File
                        userModulesInOrder =
                            topoSortModules moduleGraph userModuleNamesSet
                                |> List.filterMap (\src -> DepGraph.parseModuleName src)
                                |> List.filterMap (\name -> Dict.get name userParsedFiles)

                        -- When `normalizationRoots` is provided, only normalize
                        -- user modules that contain a function reachable — at
                        -- the function-call level — from the roots' top-level
                        -- definitions. Module-level reachability is too
                        -- coarse: for example, `tests/OrderTests.elm` imports
                        -- `Order.Extra`, which imports `String.Extra`, which
                        -- imports `String.Diacritics` — but `OrderTests`
                        -- doesn't call any Order.Extra function that
                        -- transitively touches `removeDiacritics`, so
                        -- `String.Diacritics.lookupArray` is dead code for
                        -- that test run. Function-level reachability walks
                        -- each test's function bodies and tracks the
                        -- transitive call graph.
                        normalizationReachable : Maybe (Set String)
                        normalizationReachable =
                            case config.normalizationRoots of
                                Nothing ->
                                    Nothing

                                Just rootModuleNamesList ->
                                    let
                                        seeds : Set FunctionReachability.Reference
                                        seeds =
                                            rootModuleNamesList
                                                |> List.concatMap
                                                    (\rmn ->
                                                        case Dict.get rmn userParsedFiles of
                                                            Just file ->
                                                                FunctionReachability.findTopLevelNames file
                                                                    |> Set.toList
                                                                    |> List.map (\name -> ( rmn, name ))

                                                            Nothing ->
                                                                []
                                                    )
                                                |> Set.fromList

                                        reachableFns : Set FunctionReachability.Reference
                                        reachableFns =
                                            FunctionReachability.computeReachable userParsedFiles seeds

                                        liveModules : Set String
                                        liveModules =
                                            Set.foldl (\( mn, _ ) acc -> Set.insert mn acc) Set.empty reachableFns
                                    in
                                    Just (Set.intersect liveModules userModuleNamesSet)

                        userModulesToNormalize : List File
                        userModulesToNormalize =
                            case normalizationReachable of
                                Nothing ->
                                    userModulesInOrder

                                Just reachable ->
                                    userModulesInOrder
                                        |> List.filter
                                            (\file ->
                                                case Eval.Module.fileModuleName file of
                                                    name ->
                                                        Set.member (String.join "." name) reachable
                                            )

                        envBeforeUserNormResult : Result Types.Error Eval.Module.ProjectEnv
                        envBeforeUserNormResult =
                            Eval.Module.extendWithFiles pkgEnv userModulesInOrder
                    in
                    Do.do
                        (case envBeforeUserNormResult of
                            Err _ ->
                                BackendTask.succeed Nothing

                            Ok envBeforeUserNorm ->
                                buildUserNormalizedEnv
                                    { cacheDir = config.packageParseCacheDir
                                    , packageKey = packageSummaryCacheKey allPackageSources
                                    , userModules = userModulesToNormalize
                                    , userFileContents =
                                        userFileContents
                                            |> List.filterMap
                                                (\( _, content ) ->
                                                    DepGraph.parseModuleName content
                                                        |> Maybe.map (\name -> ( name, content ))
                                                )
                                            |> Dict.fromList
                                    }
                                    envBeforeUserNorm
                                    |> BackendTask.map Just
                        )
                    <| \baseUserEnvResult ->
                    Do.do BackendTask.Time.now <| \baseUserEnvFinish ->
                    let
                        buildBaseUserEnvMs =
                            stageMs baseUserEnvStart baseUserEnvFinish
                    in
                    Do.do BackendTask.Time.now <| \semanticIndexStart ->
                    let
                        semanticIndex =
                            userFileContents
                                |> List.map
                                    (\( filePath, content ) ->
                                        { moduleName =
                                            DepGraph.parseModuleName content
                                                |> Maybe.withDefault filePath
                                        , source = content
                                        }
                                    )
                                |> SemanticHash.buildMultiModuleIndex
                    in
                    Do.do BackendTask.Time.now <| \semanticIndexFinish ->
                    let
                        buildSemanticIndexMs =
                            stageMs semanticIndexStart semanticIndexFinish
                    in
                    Do.do (withTiming (Cache.inputs allUserPaths)) <| \sourceInputsTimed ->
                    BackendTask.succeed
                        { project =
                            InterpreterProject
                                { sourceDirectories = sourceDirectories
                                , inputsByPath =
                                    sourceInputsTimed.value
                                        |> List.map
                                            (\( pathVal, monad ) ->
                                                ( Path.toString pathVal
                                                , ( pathVal, monad )
                                                )
                                            )
                                        |> Dict.fromList
                                , userFileContents =
                                    userFileContents
                                        |> Dict.fromList
                                , depGraph = depGraph
                                , patchedPackageSources = patchedPackageSources
                                , extraSources =
                                    extraFileContents
                                        |> List.map Tuple.second
                                , moduleGraph = moduleGraph
                                , packageModuleNames = pkgModuleNames
                                , packageEnv = pkgEnv
                                , baseUserEnv = baseUserEnvResult
                                , semanticIndex = semanticIndex
                                }
                        , profile =
                            { resolveSourceDirectoriesMs = sourceDirectoriesTimed.ms
                            , loadPackageSourcesMs = packageSourcesTimed.ms
                            , globUserSourcesMs = elmFilesTimed.ms
                            , readUserSourcesMs = userFileContentsTimed.ms
                            , readExtraSourcesMs = extraFileContentsTimed.ms
                            , packageSummaryCacheHit = packageSummariesInfo.packageSummaryCacheHit
                            , packageSummaryCacheRoundtripOk = packageSummariesInfo.packageSummaryCacheRoundtripOk
                            , packageSummaryCacheBytes = packageSummariesInfo.packageSummaryCacheBytes
                            , loadPackageSummaryCacheMs = packageSummariesInfo.loadPackageSummaryCacheMs
                            , decodePackageSummaryCacheMs = packageSummariesInfo.decodePackageSummaryCacheMs
                            , validatePackageSummaryCacheMs = packageSummariesInfo.validatePackageSummaryCacheMs
                            , writePackageSummaryCacheMs = packageSummariesInfo.writePackageSummaryCacheMs
                            , buildGraphMs = buildGraphMs
                            , parsePackageSourcesMs = packageSummariesInfo.parsePackageSourcesMs
                            , buildPackageSummariesFromParsedMs = packageSummariesInfo.buildPackageSummariesFromParsedMs
                            , buildPackageEnvFromSummariesMs = buildPackageEnvFromSummariesMs
                            , buildPackageEnvMs = buildPackageEnvMs
                            , buildBaseUserEnvMs = buildBaseUserEnvMs
                            , buildSemanticIndexMs = buildSemanticIndexMs
                            , cacheInputsMs = sourceInputsTimed.ms
                            }
                        }


{-| Read the "source-directories" field from elm.json in the given project directory.
-}
readSourceDirectories : Path -> BackendTask FatalError (List String)
readSourceDirectories projectDir =
    File.rawFile (Path.toString projectDir ++ "/elm.json")
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\raw ->
                case Decode.decodeString (Decode.field "source-directories" (Decode.list Decode.string)) raw of
                    Ok dirs ->
                        BackendTask.succeed dirs

                    Err err ->
                        BackendTask.fail (FatalError.fromString ("Failed to decode source-directories from elm.json: " ++ Decode.errorToString err))
            )


{-| Evaluate a `"ModuleName.valueName"` expression and cache the result.

Parses the expression to determine the module name, then delegates to
`evalWith` with that single import.

-}
eval : InterpreterProject -> String -> (FileOrDirectory -> Cache.Monad a) -> Cache.Monad a
eval project expression k =
    case parseExpression expression of
        Just ( moduleName, _ ) ->
            evalWith project { imports = [ moduleName ], expression = expression } k

        Nothing ->
            Cache.fail ("Invalid expression: " ++ expression ++ " (expected \"ModuleName.valueName\")")


{-| Evaluate an arbitrary Elm expression with multiple imports via the interpreter.

The expression can reference any of the imported modules and must produce
a `String`. Transitive dependencies are computed as the union across all
imported modules. The result is cached based on the combined hash of
package sources, extra sources, relevant user sources, and the wrapper module.

Package modules are parsed once during `loadWith` and reused across all
`evalWith` calls. Only user-specific sources are parsed per call.

-}
evalWith : InterpreterProject -> { imports : List String, expression : String } -> (FileOrDirectory -> Cache.Monad a) -> Cache.Monad a
evalWith (InterpreterProject project) { imports, expression } k =
    let
        -- Get transitive user-source deps as the union across all imported modules
        transDeps : Set String
        transDeps =
            imports
                |> List.filterMap (DepGraph.moduleNameToFilePath project.depGraph)
                |> List.map (DepGraph.transitiveDeps project.depGraph)
                |> List.foldl Set.union Set.empty

        -- Filter inputsByPath to relevant source deps
        relevantInputs : List ( String, Cache.Monad FileOrDirectory )
        relevantInputs =
            project.inputsByPath
                |> Dict.toList
                |> List.filter
                    (\( path, _ ) ->
                        Set.member path transDeps
                    )
                |> List.map (\( path, ( _, monad ) ) -> ( path, monad ))

        -- Generate wrapper module
        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        -- Compute transitive module deps from the wrapper's imports through the module graph
        wrapperImports : Set String
        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules : Set String
        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        -- Topologically sort needed modules and collect their sources
        filteredSources : List String
        filteredSources =
            topoSortModules project.moduleGraph neededModules

        -- Filter to only non-package (user) sources
        userFilteredSources : List String
        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )

        -- All sources for hashing (includes everything for correct cache keys)
        allFilteredSources : List String
        allFilteredSources =
            filteredSources ++ [ wrapperSource ]

        -- Single blob of all filtered sources for hashing
        filteredBlob : String
        filteredBlob =
            String.join "\n---MODULE_SEPARATOR---\n" allFilteredSources
    in
    -- Hash the filtered blob
    Cache.do (Cache.writeFile filteredBlob Cache.succeed) <| \filteredHash ->
    -- Resolve all relevant user file hashes
    Cache.do
        (relevantInputs
            |> List.map
                (\( path, monad ) ->
                    Cache.do monad <| \hash ->
                    Cache.succeed { filename = Path.path path, hash = hash }
                )
            |> Cache.sequence
        )
    <| \sourceFiles ->
    -- Combine all hashes into a single combined hash
    Cache.do
        (Cache.combine
            (sourceFiles
                ++ [ { filename = Path.path "filtered.blob", hash = filteredHash }
                   ]
            )
        )
    <| \combinedHash ->
    -- Cache the interpreter computation
    Cache.compute [ "interpret" ]
        combinedHash
        (\() ->
            let
                -- Only parse user sources + wrapper; reuse the pre-built package env
                result : Result Types.Error Types.Value
                result =
                    Eval.Module.evalWithEnv
                        project.packageEnv
                        (userFilteredSources ++ [ wrapperSource ])
                        (FunctionOrValue [] "results")
            in
            case result of
                Ok (Types.String s) ->
                    s

                Ok other ->
                    "ERROR: Expected String result"

                Err (Types.ParsingError _) ->
                    "ERROR: Parsing error"

                Err (Types.EvalError evalErr) ->
                    "ERROR: Eval error: "
                        ++ evalErrorKindToString evalErr.error
                        ++ " [module: "
                        ++ String.join "." evalErr.currentModule
                        ++ "] [stack: "
                        ++ (evalErr.callStack |> List.take 10 |> List.map (\ref -> String.join "." ref.moduleName ++ "." ++ ref.name) |> String.join " <- ")
                        ++ "]"
        )
        k



{-| Evaluate an expression with source overrides.

Like `evalWith`, but allows overriding specific module sources before
evaluation. Override sources take precedence over existing modules with
the same name. Useful for mutation testing, where you want to evaluate
the same expression with a modified version of one module.

The cache key includes the override source hashes, so each unique
mutation gets its own cached result.

-}
evalWithSourceOverrides : InterpreterProject -> { imports : List String, expression : String, sourceOverrides : List String } -> (FileOrDirectory -> Cache.Monad a) -> Cache.Monad a
evalWithSourceOverrides (InterpreterProject project) { imports, expression, sourceOverrides } k =
    let
        transDeps : Set String
        transDeps =
            imports
                |> List.filterMap (DepGraph.moduleNameToFilePath project.depGraph)
                |> List.map (DepGraph.transitiveDeps project.depGraph)
                |> List.foldl Set.union Set.empty

        relevantInputs : List ( String, Cache.Monad FileOrDirectory )
        relevantInputs =
            project.inputsByPath
                |> Dict.toList
                |> List.filter (\( path, _ ) -> Set.member path transDeps)
                |> List.map (\( path, ( _, monad ) ) -> ( path, monad ))

        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        wrapperImports : Set String
        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules : Set String
        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        filteredSources : List String
        filteredSources =
            topoSortModules project.moduleGraph neededModules

        userFilteredSources : List String
        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )

        -- Include overrides in the content blob for hashing
        allFilteredSources : List String
        allFilteredSources =
            filteredSources ++ sourceOverrides ++ [ wrapperSource ]

        filteredBlob : String
        filteredBlob =
            String.join "\n---MODULE_SEPARATOR---\n" allFilteredSources
    in
    Cache.do (Cache.writeFile filteredBlob Cache.succeed) <| \filteredHash ->
    Cache.do
        (relevantInputs
            |> List.map
                (\( path, monad ) ->
                    Cache.do monad <| \hash ->
                    Cache.succeed { filename = Path.path path, hash = hash }
                )
            |> Cache.sequence
        )
    <| \sourceFiles ->
    Cache.do
        (Cache.combine
            (sourceFiles
                ++ [ { filename = Path.path "filtered.blob", hash = filteredHash } ]
            )
        )
    <| \combinedHash ->
    Cache.compute [ "interpret-with-overrides" ]
        combinedHash
        (\() ->
            let
                -- User sources first, then overrides (which replace same-named
                -- modules since evalWithEnv processes in order, last wins).
                -- Wrapper goes last for import resolution.
                result : Result Types.Error Types.Value
                result =
                    Eval.Module.evalWithEnv
                        project.packageEnv
                        (userFilteredSources ++ sourceOverrides ++ [ wrapperSource ])
                        (FunctionOrValue [] "results")
            in
            case result of
                Ok (Types.String s) ->
                    s

                Ok _ ->
                    "ERROR: Expected String result"

                Err (Types.ParsingError _) ->
                    "ERROR: Parsing error"

                Err (Types.EvalError evalErr) ->
                    "ERROR: Eval error: "
                        ++ evalErrorKindToString evalErr.error
                        ++ " [module: "
                        ++ String.join "." evalErr.currentModule
                        ++ "] [stack: "
                        ++ (evalErr.callStack |> List.take 10 |> List.map (\ref -> String.join "." ref.moduleName ++ "." ++ ref.name) |> String.join " <- ")
                        ++ "]"
        )
        k



{-| Evaluate an expression with pre-parsed File AST overrides.

Like `evalWithSourceOverrides`, but accepts `File` ASTs directly instead of
source strings for the overrides. Skips the write→parse round-trip for mutations.

The `sourceOverrides` are the string sources that still need parsing (e.g. SimpleTestRunner).
The `fileOverrides` are pre-parsed ASTs (e.g. mutated File from Mutator).

-}
evalWithFileOverrides :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , fileOverrides : List { file : File, hashKey : String }
        }
    -> (FileOrDirectory -> Cache.Monad a)
    -> Cache.Monad a
evalWithFileOverrides (InterpreterProject project) { imports, expression, sourceOverrides, fileOverrides } k =
    let
        transDeps : Set String
        transDeps =
            imports
                |> List.filterMap (DepGraph.moduleNameToFilePath project.depGraph)
                |> List.map (DepGraph.transitiveDeps project.depGraph)
                |> List.foldl Set.union Set.empty

        relevantInputs : List ( String, Cache.Monad FileOrDirectory )
        relevantInputs =
            project.inputsByPath
                |> Dict.toList
                |> List.filter (\( path, _ ) -> Set.member path transDeps)
                |> List.map (\( path, ( _, monad ) ) -> ( path, monad ))

        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        wrapperImports : Set String
        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules : Set String
        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        filteredSources : List String
        filteredSources =
            topoSortModules project.moduleGraph neededModules

        userFilteredSources : List String
        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )

        -- Pre-parsed user module Files (from loadWith), in topo order
        userFilteredFiles : List File
        userFilteredFiles =
            neededModules
                |> Set.toList
                |> List.filter (\name -> not (Set.member name project.packageModuleNames))
                |> List.filterMap (\name -> Dict.get name project.moduleGraph.moduleToFile)

        -- Semantic hash: hash the wrapper expression's semantic dependencies
        -- + override hash keys. Only invalidates when actually-called functions change.
        fileOverrideHashKeys : List String
        fileOverrideHashKeys =
            fileOverrides |> List.map .hashKey

        semanticCacheKey : String
        semanticCacheKey =
            let
                -- Extract function references from the wrapper expression to find
                -- entry-point declarations, then use their semantic hashes (which
                -- transitively include all dependencies via the Merkle property)
                wrapperDeps =
                    case Elm.Parser.parseToFile wrapperSource of
                        Ok wrapperFile ->
                            wrapperFile.declarations
                                |> List.concatMap
                                    (\(Node _ decl) ->
                                        case decl of
                                            FunctionDeclaration func ->
                                                SemanticHash.extractDependencies
                                                    (Node
                                                        { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } }
                                                        (Node.value func.declaration |> .expression |> Node.value)
                                                    )

                                            _ ->
                                                []
                                    )

                        Err _ ->
                            []

                -- Resolve each dependency to its semantic hash from the index
                entryPointHashes =
                    wrapperDeps
                        |> List.filterMap
                            (\( modName, funcName ) ->
                                let
                                    qualName =
                                        if List.isEmpty modName then
                                            funcName

                                        else
                                            String.join "." modName ++ "." ++ funcName
                                in
                                SemanticHash.semanticHashForEntry project.semanticIndex qualName
                            )
                        |> List.sort

                overrideKey =
                    String.join "|" fileOverrideHashKeys

                sourceOverrideKey =
                    String.join "|" (List.map (\s -> String.left 100 s) sourceOverrides)
            in
            String.join "\n"
                (entryPointHashes ++ [ overrideKey, sourceOverrideKey, wrapperSource ])
    in
    Cache.do (Cache.writeFile semanticCacheKey Cache.succeed) <| \semanticHash ->
    Cache.compute [ "interpret-with-file-overrides" ]
        semanticHash
        (\() ->
            let
                -- Only parse sourceOverrides and wrapper (small/new);
                -- user sources are pre-parsed in moduleGraph.moduleToFile
                parsedOverrides : Result Types.Error (List File)
                parsedOverrides =
                    (sourceOverrides ++ [ wrapperSource ])
                        |> List.map
                            (\src ->
                                Elm.Parser.parseToFile src
                                    |> Result.mapError Types.ParsingError
                            )
                        |> combineFileResults
            in
            case parsedOverrides of
                Err err ->
                    case err of
                        Types.ParsingError _ ->
                            "ERROR: Parsing error"

                        Types.EvalError evalErr ->
                            "ERROR: Eval error: " ++ evalErrorKindToString evalErr.error

                Ok overrideFiles ->
                    let
                        overridesButWrapper =
                            List.take (List.length overrideFiles - 1) overrideFiles

                        wrapperFile =
                            List.drop (List.length overrideFiles - 1) overrideFiles

                        -- Incremental env: if baseUserEnv is available, replace only
                        -- the mutated module(s) instead of rebuilding from all user files.
                        result : Result Types.Error Types.Value
                        result =
                            case project.baseUserEnv of
                                Just baseEnv ->
                                    let
                                        -- Replace each file override in the base env
                                        replacedEnvResult =
                                            fileOverrides
                                                |> List.foldl
                                                    (\override envRes ->
                                                        envRes
                                                            |> Result.andThen
                                                                (\env ->
                                                                    Eval.Module.replaceModuleInEnv env
                                                                        { file = override.file
                                                                        , moduleName = Eval.Module.fileModuleName override.file
                                                                        , interface = Eval.Module.buildInterfaceFromFile override.file
                                                                        }
                                                                )
                                                    )
                                                    (Ok baseEnv)

                                        -- Only need sourceOverrides + wrapper as additional files
                                        additionalFiles =
                                            overridesButWrapper ++ wrapperFile
                                    in
                                    case replacedEnvResult of
                                        Ok updatedEnv ->
                                            evalAdditionalFiles updatedEnv additionalFiles fileOverrides

                                        Err e ->
                                            Err e

                                Nothing ->
                                    -- Fallback: original path (all user files)
                                    let
                                        allFiles =
                                            userFilteredFiles
                                                ++ overridesButWrapper
                                                ++ List.map .file fileOverrides
                                                ++ wrapperFile
                                    in
                                    evalAdditionalFiles project.packageEnv allFiles fileOverrides
                    in
                    case result of
                        Ok (Types.String s) ->
                            s

                        Ok _ ->
                            "ERROR: Expected String result"

                        Err (Types.ParsingError _) ->
                            "ERROR: Parsing error"

                        Err (Types.EvalError evalErr) ->
                            "ERROR: Eval error: "
                                ++ evalErrorKindToString evalErr.error
        )
        k


{-| Run the test runner's `results` expression against an extended
project env. Routes through the resolved-IR evaluator
(`evalWithResolvedIRFromFilesAndIntercepts`) when there are no file
overrides — the test-runner cold path — and falls back to the old
string-keyed evaluator when there are. The mutation-testing path uses
file overrides + `replaceModuleInEnv` which doesn't currently refresh
the precomputed `ResolvedProject`, so feeding it through the resolved-
IR evaluator would see stale function bodies. The fallback is safe
and the dev-loop scenarios it covers are already at parity with
elm-test.

-}
evalAdditionalFiles :
    Eval.Module.ProjectEnv
    -> List File
    -> List { file : File, hashKey : String }
    -> Result Types.Error Types.Value
evalAdditionalFiles env additionalFiles fileOverrides =
    if List.isEmpty fileOverrides then
        case
            Eval.Module.evalWithResolvedIRFromFilesAndIntercepts
                env
                additionalFiles
                FastDict.empty
                FastDict.empty
                (FunctionOrValue [] "results")
        of
            Types.EvOk value ->
                Ok value

            Types.EvErr errorData ->
                Err (Types.EvalError errorData)

            _ ->
                Err
                    (Types.EvalError
                        { currentModule = []
                        , callStack = []
                        , error = Types.Unsupported "evalWithResolvedIRFromFilesAndIntercepts returned a trace/yield/memo result; not supported on the test-runner path"
                        }
                    )

    else
        Eval.Module.evalWithEnvFromFilesAndLimit
            (Just 5000000)
            env
            additionalFiles
            (FunctionOrValue [] "results")


{-| Evaluate a test expression with tracing to collect coverage data.

Runs the test suite once (unmutated) with the interpreter's trace mode enabled,
then walks the resulting CallTree to extract all evaluated source ranges.
Returns both the test result string and the list of covered ranges.

-}
evalSimple :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        }
    -> BackendTask FatalError String
evalSimple (InterpreterProject project) { imports, expression, sourceOverrides } =
    let
        wrapperSource =
            generateWrapper imports expression

        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        filteredSources =
            topoSortModules project.moduleGraph neededModules

        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )

        allSources =
            userFilteredSources ++ sourceOverrides ++ [ wrapperSource ]

        result =
            Eval.Module.evalWithEnv
                project.packageEnv
                allSources
                (FunctionOrValue [] "results")
    in
    BackendTask.succeed
        (case result of
            Ok (Types.String s) ->
                s

            Ok _ ->
                "ERROR: Expected String result"

            Err (Types.ParsingError _) ->
                "ERROR: Parsing error"

            Err (Types.EvalError evalErr) ->
                "ERROR: Eval error: " ++ evalErrorKindToString evalErr.error
        )


evalWithCoverage :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , probeLines : Set Int
        }
    -> BackendTask FatalError { result : String, coveredRanges : List Range }
evalWithCoverage (InterpreterProject project) { imports, expression, sourceOverrides, probeLines } =
    let
        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        wrapperImports : Set String
        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules : Set String
        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        filteredSources : List String
        filteredSources =
            topoSortModules project.moduleGraph neededModules

        userFilteredSources : List String
        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )
    in
    let
        allSources =
            userFilteredSources ++ sourceOverrides ++ [ wrapperSource ]

        ( result, coveredRanges ) =
            Eval.Module.coverageWithEnvAndLimit Nothing
                probeLines
                project.packageEnv
                allSources
                (FunctionOrValue [] "results")

        resultString =
            case result of
                Ok (Types.String s) ->
                    s

                Ok _ ->
                    "ERROR: Expected String result"

                Err (Types.ParsingError _) ->
                    "ERROR: Parsing error"

                Err (Types.EvalError evalErr) ->
                    "ERROR: Eval error: " ++ evalErrorKindToString evalErr.error
    in
    BackendTask.succeed { result = resultString, coveredRanges = coveredRanges }


combineFileResults : List (Result e a) -> Result e (List a)
combineFileResults results =
    List.foldr
        (\result acc ->
            Result.map2 (::) result acc
        )
        (Ok [])
        results


evalErrorKindToString : Types.EvalErrorKind -> String
evalErrorKindToString kind =
    case kind of
        Types.TypeError msg ->
            "type error: " ++ msg

        Types.Unsupported msg ->
            "unsupported: " ++ msg

        Types.NameError name ->
            "could not resolve '" ++ name ++ "'"

        Types.Todo msg ->
            "hit Debug.todo: " ++ msg

        Types.TailCall _ ->
            "internal TCO signal (should not be user-visible)"


{-| Get the pre-built package environment. Useful for direct eval calls
that bypass the caching layer (e.g. benchmarking).
-}
getPackageEnv : InterpreterProject -> Eval.Module.ProjectEnv
getPackageEnv (InterpreterProject project) =
    project.packageEnv


precomputedValuesCount : InterpreterProject -> Int
precomputedValuesCount (InterpreterProject project) =
    case project.baseUserEnv of
        Just env ->
            Eval.Module.precomputedValuesCount env

        Nothing ->
            Eval.Module.precomputedValuesCount project.packageEnv


precomputedValuesByModule : InterpreterProject -> List ( String, Int )
precomputedValuesByModule (InterpreterProject project) =
    case project.baseUserEnv of
        Just env ->
            Eval.Module.precomputedValuesByModule env

        Nothing ->
            Eval.Module.precomputedValuesByModule project.packageEnv


{-| Get the dependency graph for the project's user source files.
-}
getDepGraph : InterpreterProject -> DepGraph.Graph
getDepGraph (InterpreterProject project) =
    project.depGraph


{-| Prepare the source lists needed for eval, without actually evaluating.

Returns `allSources` (everything needed, for `evalProject`) and
`userSources` (only non-package modules, for `evalWithEnv`).

-}
prepareEvalSources :
    InterpreterProject
    -> { imports : List String, expression : String }
    -> { allSources : List String, userSources : List String }
prepareEvalSources (InterpreterProject project) { imports, expression } =
    let
        wrapperSource =
            generateWrapper imports expression

        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        filteredSources =
            topoSortModules project.moduleGraph neededModules

        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )
    in
    { allSources = filteredSources ++ [ wrapperSource ]
    , userSources = userFilteredSources ++ [ wrapperSource ]
    }


{-| Prepare and evaluate an expression, pure function without Cache.Monad.

Uses baseUserEnv when available (skips re-parsing all user modules).
Only parses sourceOverrides + the generated wrapper module.

Useful when the caller manages caching externally (e.g. using semantic hash
keys instead of content-based hashing).

-}
prepareAndEval :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String }
    -> Result String String
prepareAndEval (InterpreterProject project) { imports, expression, sourceOverrides } =
    let
        wrapperSource =
            generateWrapper imports expression

        -- Parse only sourceOverrides + wrapper (small, new each time).
        -- User modules are already in baseUserEnv.
        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults

        result =
            case parsedNewModules of
                Err err ->
                    Err err

                Ok newFiles ->
                    case project.baseUserEnv of
                        Just baseEnv ->
                            -- Fast path: baseUserEnv already has all user modules.
                            -- Only need to add sourceOverrides + wrapper.
                            Eval.Module.evalWithEnvFromFiles baseEnv newFiles (FunctionOrValue [] "results")

                        Nothing ->
                            -- Fallback: parse everything from scratch
                            let
                                { userSources } =
                                    prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                                allSources =
                                    let
                                        len =
                                            List.length userSources

                                        beforeWrapper =
                                            List.take (len - 1) userSources

                                        wrapper =
                                            List.drop (len - 1) userSources
                                    in
                                    beforeWrapper ++ sourceOverrides ++ wrapper
                            in
                            Eval.Module.evalWithEnv project.packageEnv allSources (FunctionOrValue [] "results")
    in
    formatEvalResult result


{-| Like prepareAndEval but returns the raw interpreter Value.

Useful when the expression returns structured data (e.g. a Tuple)
that the caller needs to decompose.

-}
prepareAndEvalRaw :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String }
    -> Result String Types.Value
prepareAndEvalRaw (InterpreterProject project) { imports, expression, sourceOverrides } =
    let
        wrapperSource =
            generateWrapper imports expression

        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults
    in
    case parsedNewModules of
        Err err ->
            Err (formatError err)

        Ok newFiles ->
            case project.baseUserEnv of
                Just baseEnv ->
                    Eval.Module.evalWithEnvFromFiles baseEnv newFiles (FunctionOrValue [] "results")
                        |> Result.mapError formatError

                Nothing ->
                    let
                        { userSources } =
                            prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                        allSources =
                            let
                                len =
                                    List.length userSources

                                beforeWrapper =
                                    List.take (len - 1) userSources

                                wrapper =
                                    List.drop (len - 1) userSources
                            in
                            beforeWrapper ++ sourceOverrides ++ wrapper
                    in
                    Eval.Module.evalWithEnv project.packageEnv allSources (FunctionOrValue [] "results")
                        |> Result.mapError formatError


{-| Like prepareAndEval but with injected Values available in the expression.

The injected values are available as local variables. Used to pass
preserved interpreter state (like elm-review's updatedRules) into
subsequent evaluations.

-}
prepareAndEvalWithValues :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, injectedValues : FastDict.Dict String Types.Value }
    -> Result String Types.Value
prepareAndEvalWithValues (InterpreterProject project) { imports, expression, sourceOverrides, injectedValues } =
    let
        wrapperSource =
            generateWrapper imports expression

        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults
    in
    case parsedNewModules of
        Err err ->
            Err (formatError err)

        Ok newFiles ->
            case project.baseUserEnv of
                Just baseEnv ->
                    Eval.Module.evalWithEnvFromFilesAndValues baseEnv newFiles injectedValues (FunctionOrValue [] "results")
                        |> Result.mapError formatError

                Nothing ->
                    -- Fallback without values (can't inject without baseUserEnv)
                    Eval.Module.evalWithEnvFromFiles project.packageEnv newFiles (FunctionOrValue [] "results")
                        |> Result.mapError formatError


{-| Like `prepareAndEvalRaw`, but uses the interpreter's internal memo runtime
for selected qualified function names and returns the updated memo cache so it
can be reused across later invocations.
-}
prepareAndEvalWithMemoizedFunctions :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , memoizedFunctions : Set String
        , memoCache : MemoRuntime.MemoCache
        , collectMemoStats : Bool
        }
    ->
        Result String
            { value : Types.Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
prepareAndEvalWithMemoizedFunctions (InterpreterProject project) { imports, expression, sourceOverrides, memoizedFunctions, memoCache, collectMemoStats } =
    let
        wrapperSource =
            generateWrapper imports expression

        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults
    in
    case parsedNewModules of
        Err err ->
            Err (formatError err)

        Ok newFiles ->
            case project.baseUserEnv of
                Just baseEnv ->
                    Eval.Module.evalWithEnvFromFilesAndMemo baseEnv newFiles memoizedFunctions memoCache collectMemoStats (FunctionOrValue [] "results")
                        |> Result.mapError formatError

                Nothing ->
                    let
                        { userSources } =
                            prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                        allSources =
                            let
                                len =
                                    List.length userSources

                                beforeWrapper =
                                    List.take (len - 1) userSources

                                wrapper =
                                    List.drop (len - 1) userSources
                            in
                            beforeWrapper ++ sourceOverrides ++ wrapper
                    in
                    Eval.Module.evalWithMemoizedFunctions project.packageEnv allSources memoizedFunctions memoCache collectMemoStats (FunctionOrValue [] "results")
                        |> Result.mapError formatError


prepareAndEvalWithValuesAndMemoizedFunctions :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , injectedValues : FastDict.Dict String Types.Value
        , memoizedFunctions : Set String
        , memoCache : MemoRuntime.MemoCache
        , collectMemoStats : Bool
        }
    ->
        Result String
            { value : Types.Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
prepareAndEvalWithValuesAndMemoizedFunctions (InterpreterProject project) { imports, expression, sourceOverrides, injectedValues, memoizedFunctions, memoCache, collectMemoStats } =
    let
        wrapperSource =
            generateWrapper imports expression

        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults
    in
    case parsedNewModules of
        Err err ->
            Err (formatError err)

        Ok newFiles ->
            case project.baseUserEnv of
                Just baseEnv ->
                    Eval.Module.evalWithEnvFromFilesAndValuesAndMemo baseEnv newFiles injectedValues memoizedFunctions memoCache collectMemoStats (FunctionOrValue [] "results")
                        |> Result.mapError formatError

                Nothing ->
                    let
                        { userSources } =
                            prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                        allSources =
                            let
                                len =
                                    List.length userSources

                                beforeWrapper =
                                    List.take (len - 1) userSources

                                wrapper =
                                    List.drop (len - 1) userSources
                            in
                            beforeWrapper ++ sourceOverrides ++ wrapper
                    in
                    Eval.Module.evalWithValuesAndMemoizedFunctions project.packageEnv allSources injectedValues memoizedFunctions memoCache collectMemoStats (FunctionOrValue [] "results")
                        |> Result.mapError formatError


{-| Evaluate with intercepts that can yield to the framework.

When an intercept yields (EvYield tag payload resume), the yieldHandler
BackendTask runs with (tag, payload), producing a Value. The eval then
resumes with that Value via the continuation.

This is the BackendTask driver loop: eval → yield → handle → resume → repeat.
-}
prepareRawEvalWithYield :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, intercepts : FastDict.Dict String Types.Intercept, injectedValues : FastDict.Dict String Types.Value }
    -> Types.EvalResult Types.Value
prepareRawEvalWithYield (InterpreterProject project) { imports, expression, sourceOverrides, intercepts, injectedValues } =
    prepareRawEvalWithYieldAndMemo
        (InterpreterProject project)
        { imports = imports
        , expression = expression
        , sourceOverrides = sourceOverrides
        , intercepts = intercepts
        , injectedValues = injectedValues
        , memoizedFunctions = Set.empty
        }


prepareRawEvalWithYieldAndMemo :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , intercepts : FastDict.Dict String Types.Intercept
        , injectedValues : FastDict.Dict String Types.Value
        , memoizedFunctions : Set String
        }
    -> Types.EvalResult Types.Value
prepareRawEvalWithYieldAndMemo (InterpreterProject project) { imports, expression, sourceOverrides, intercepts, injectedValues, memoizedFunctions } =
    let
        wrapperSource =
            generateWrapper imports expression

        allSources =
            let
                { userSources } =
                    prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                len =
                    List.length userSources

                beforeWrapper =
                    List.take (len - 1) userSources

                wrapper =
                    List.drop (len - 1) userSources
            in
            beforeWrapper ++ sourceOverrides ++ wrapper

        evalSources =
            case project.baseUserEnv of
                Just _ ->
                    sourceOverrides ++ [ wrapperSource ]

                Nothing ->
                    allSources

        env =
            case project.baseUserEnv of
                Just baseEnv ->
                    baseEnv

                Nothing ->
                    project.packageEnv

        rawResult =
            if useResolvedIRPath && Set.isEmpty memoizedFunctions then
                let
                    parseResult =
                        evalSources
                            |> List.map
                                (\src ->
                                    Elm.Parser.parseToFile src
                                        |> Result.mapError Types.ParsingError
                                )
                            |> combineFileResults
                in
                case parseResult of
                    Err _ ->
                        Types.EvErr { currentModule = [], callStack = [], error = Types.TypeError "Parse error in prepareAndEvalWithYield (resolved IR path)" }

                    Ok parsedModules ->
                        Eval.Module.evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw
                            env
                            parsedModules
                            injectedValues
                            intercepts
                            Set.empty
                            (FunctionOrValue [] "results")

            else if FastDict.isEmpty injectedValues then
                Eval.Module.evalWithInterceptsAndMemoRaw env evalSources intercepts memoizedFunctions (FunctionOrValue [] "results")

            else
                -- Need to inject values AND use intercepts.
                -- Parse sources, build env, inject values, then eval with intercepts.
                -- This is a combination of evalWithEnvFromFilesAndValues + evalWithInterceptsRaw.
                let
                    parseResult =
                        evalSources
                            |> List.map
                                (\src ->
                                    Elm.Parser.parseToFile src
                                        |> Result.mapError Types.ParsingError
                                )
                            |> combineFileResults
                in
                case parseResult of
                    Err _ ->
                        Types.EvErr { currentModule = [], callStack = [], error = Types.TypeError "Parse error in prepareAndEvalWithYield" }

                    Ok parsedModules ->
                        Eval.Module.evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw env parsedModules injectedValues intercepts memoizedFunctions (FunctionOrValue [] "results")
    in
    rawResult


{-| Phase 4 Round 2 gate: when True, route yield-driving eval calls
through the new resolved-IR evaluator instead of the old string-keyed
path. Falls back to the old path when `memoizedFunctions` is non-empty
since memo support hasn't been ported yet.

Flip this to `False` to A/B the two paths during benchmarking. When
Phase 4 Round 3 lands (deletion of the old evaluator), this constant
goes away.

-}
useResolvedIRPath : Bool
useResolvedIRPath =
    True


{-| Convert the `EvalResult Value` returned by the new
`evalWithResolvedIRFromFilesAndIntercepts` entry point into the
`Result String Value` shape callers of `prepareAndEvalWithIntercepts`
expect. Only the synchronous `EvOk` / `EvErr` variants can be
translated directly; an intercept that returns `EvYield` / `EvMemoLookup`
/ `EvMemoStore` through this synchronous path would be a programming
error (no yield handler is installed), so we surface it as a formatted
error string to make it obvious if it happens.

-}
resolvedEvalResultToResult : Types.EvalResult Types.Value -> Result String Types.Value
resolvedEvalResultToResult result =
    case result of
        Types.EvOk value ->
            Ok value

        Types.EvErr errorData ->
            Err
                (formatError
                    (Types.EvalError errorData)
                )

        Types.EvOkTrace value _ _ ->
            Ok value

        Types.EvErrTrace errorData _ _ ->
            Err (formatError (Types.EvalError errorData))

        Types.EvYield tag _ _ ->
            Err ("ERROR: Unexpected EvYield '" ++ tag ++ "' in synchronous intercept path (resolvedEvalResultToResult)")

        Types.EvMemoLookup _ _ ->
            Err "ERROR: Unexpected EvMemoLookup in synchronous intercept path (resolvedEvalResultToResult)"

        Types.EvMemoStore _ _ ->
            Err "ERROR: Unexpected EvMemoStore in synchronous intercept path (resolvedEvalResultToResult)"

        Types.EvOkCoverage value _ ->
            Ok value

        Types.EvErrCoverage errorData _ ->
            Err (formatError (Types.EvalError errorData))


{-| Evaluate with intercepts that can yield to the framework.

When an intercept yields (EvYield tag payload resume), the yieldHandler
BackendTask runs with (tag, payload), producing a Value. The eval then
resumes with that Value via the continuation.

This is the BackendTask driver loop: eval → yield → handle → resume → repeat.
-}
prepareAndEvalWithYield :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, intercepts : FastDict.Dict String Types.Intercept, injectedValues : FastDict.Dict String Types.Value }
    -> (String -> Types.Value -> BackendTask FatalError Types.Value)
    -> BackendTask FatalError (Result String Types.Value)
prepareAndEvalWithYield project evalConfig yieldHandler =
    driveYields yieldHandler (prepareRawEvalWithYield project evalConfig)


prepareAndEvalWithYieldAndMemoizedFunctions :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , intercepts : FastDict.Dict String Types.Intercept
        , injectedValues : FastDict.Dict String Types.Value
        , memoizedFunctions : Set String
        , memoCache : MemoRuntime.MemoCache
        , collectMemoStats : Bool
        }
    -> (String -> Types.Value -> BackendTask FatalError Types.Value)
    ->
        BackendTask FatalError
            { result : Result String Types.Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
prepareAndEvalWithYieldAndMemoizedFunctions project evalConfig yieldHandler =
    driveYieldsAndMemo
        evalConfig.memoCache
        (if evalConfig.collectMemoStats then
            MemoRuntime.emptyMemoStats

         else
            MemoRuntime.disabledMemoStats
        )
        yieldHandler
        (prepareRawEvalWithYieldAndMemo project
            { imports = evalConfig.imports
            , expression = evalConfig.expression
            , sourceOverrides = evalConfig.sourceOverrides
            , intercepts = evalConfig.intercepts
            , injectedValues = evalConfig.injectedValues
            , memoizedFunctions = evalConfig.memoizedFunctions
            }
        )


{-| Like `prepareAndEvalWithYield`, but threads caller-managed state through the
yield loop. Useful for in-memory caching experiments where the state should
live outside the interpreter and be updated on each yield.
-}
prepareAndEvalWithYieldState :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, intercepts : FastDict.Dict String Types.Intercept, injectedValues : FastDict.Dict String Types.Value }
    -> state
    -> (state -> String -> Types.Value -> BackendTask FatalError ( state, Types.Value ))
    -> BackendTask FatalError ( Result String Types.Value, state )
prepareAndEvalWithYieldState project evalConfig initialState yieldHandler =
    driveYieldsState initialState yieldHandler (prepareRawEvalWithYield project evalConfig)


{-| Drive the yield loop: handle EvYield via BackendTask, resume, repeat.
-}
driveYields :
    (String -> Types.Value -> BackendTask FatalError Types.Value)
    -> Types.EvalResult Types.Value
    -> BackendTask FatalError (Result String Types.Value)
driveYields yieldHandler evalResult =
    case evalResult of
        Types.EvOk value ->
            BackendTask.succeed (Ok value)

        Types.EvErr evalErr ->
            BackendTask.succeed (Err (formatError (Types.EvalError evalErr)))

        Types.EvOkTrace value _ _ ->
            BackendTask.succeed (Ok value)

        Types.EvErrTrace evalErr _ _ ->
            BackendTask.succeed (Err (formatError (Types.EvalError evalErr)))

        Types.EvMemoLookup _ resume ->
            driveYields yieldHandler (resume Nothing)

        Types.EvMemoStore _ next ->
            driveYields yieldHandler next

        Types.EvYield tag payload resume ->
            -- Handle the yield via BackendTask, then resume eval
            yieldHandler tag payload
                |> BackendTask.andThen
                    (\resumeValue ->
                        driveYields yieldHandler (resume resumeValue)
                    )

        Types.EvOkCoverage value _ ->
            BackendTask.succeed (Ok value)

        Types.EvErrCoverage evalErr _ ->
            BackendTask.succeed (Err (formatError (Types.EvalError evalErr)))


driveYieldsState :
    state
    -> (state -> String -> Types.Value -> BackendTask FatalError ( state, Types.Value ))
    -> Types.EvalResult Types.Value
    -> BackendTask FatalError ( Result String Types.Value, state )
driveYieldsState state yieldHandler evalResult =
    case evalResult of
        Types.EvOk value ->
            BackendTask.succeed ( Ok value, state )

        Types.EvErr evalErr ->
            BackendTask.succeed ( Err (formatError (Types.EvalError evalErr)), state )

        Types.EvOkTrace value _ _ ->
            BackendTask.succeed ( Ok value, state )

        Types.EvErrTrace evalErr _ _ ->
            BackendTask.succeed ( Err (formatError (Types.EvalError evalErr)), state )

        Types.EvMemoLookup _ resume ->
            driveYieldsState state yieldHandler (resume Nothing)

        Types.EvMemoStore _ next ->
            driveYieldsState state yieldHandler next

        Types.EvYield tag payload resume ->
            yieldHandler state tag payload
                |> BackendTask.andThen
                    (\( nextState, resumeValue ) ->
                        driveYieldsState nextState yieldHandler (resume resumeValue)
                    )

        Types.EvOkCoverage value _ ->
            BackendTask.succeed ( Ok value, state )

        Types.EvErrCoverage evalErr _ ->
            BackendTask.succeed ( Err (formatError (Types.EvalError evalErr)), state )


driveYieldsAndMemo :
    MemoRuntime.MemoCache
    -> MemoRuntime.MemoStats
    -> (String -> Types.Value -> BackendTask FatalError Types.Value)
    -> Types.EvalResult Types.Value
    ->
        BackendTask FatalError
            { result : Result String Types.Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
driveYieldsAndMemo memoCache memoStats yieldHandler evalResult =
    case evalResult of
        Types.EvOk value ->
            BackendTask.succeed
                { result = Ok value
                , memoCache = memoCache
                , memoStats = memoStats
                }

        Types.EvErr evalErr ->
            BackendTask.succeed
                { result = Err (formatError (Types.EvalError evalErr))
                , memoCache = memoCache
                , memoStats = memoStats
                }

        Types.EvOkTrace value _ _ ->
            BackendTask.succeed
                { result = Ok value
                , memoCache = memoCache
                , memoStats = memoStats
                }

        Types.EvErrTrace evalErr _ _ ->
            BackendTask.succeed
                { result = Err (formatError (Types.EvalError evalErr))
                , memoCache = memoCache
                , memoStats = memoStats
                }

        Types.EvMemoLookup payload resume ->
            let
                ( nextCache, nextStats, maybeValue ) =
                    Eval.Module.handleInternalMemoLookup memoCache memoStats payload
            in
            driveYieldsAndMemo nextCache nextStats yieldHandler (resume maybeValue)

        Types.EvMemoStore payload next ->
            let
                ( nextCache, nextStats ) =
                    Eval.Module.handleInternalMemoStore memoCache memoStats payload
            in
            driveYieldsAndMemo nextCache nextStats yieldHandler next

        Types.EvYield tag payload resume ->
            case Eval.Module.handleInternalMemoYield memoCache memoStats tag payload of
                Just ( nextCache, nextStats, resumeValue ) ->
                    driveYieldsAndMemo nextCache nextStats yieldHandler (resume resumeValue)

                Nothing ->
                    yieldHandler tag payload
                        |> BackendTask.andThen
                            (\resumeValue ->
                                driveYieldsAndMemo memoCache memoStats yieldHandler (resume resumeValue)
                            )

        Types.EvOkCoverage value _ ->
            BackendTask.succeed
                { result = Ok value
                , memoCache = memoCache
                , memoStats = memoStats
                }

        Types.EvErrCoverage evalErr _ ->
            BackendTask.succeed
                { result = Err (formatError (Types.EvalError evalErr))
                , memoCache = memoCache
                , memoStats = memoStats
                }


{-| Like prepareAndEval but with function intercepts (synchronous, no yield support).

Intercepts are checked before normal function evaluation. Used for
elm-review cache markers, memoization, and framework callbacks.
-}
prepareAndEvalWithIntercepts :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, intercepts : FastDict.Dict String Types.Intercept }
    -> Result String Types.Value
prepareAndEvalWithIntercepts (InterpreterProject project) { imports, expression, sourceOverrides, intercepts } =
    let
        wrapperSource =
            generateWrapper imports expression

        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults
    in
    case parsedNewModules of
        Err err ->
            Err (formatError err)

        Ok newFiles ->
            let
                -- Build sources list for evalWithIntercepts
                { userSources } =
                    prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                allSources =
                    let
                        len =
                            List.length userSources

                        beforeWrapper =
                            List.take (len - 1) userSources

                        wrapper =
                            List.drop (len - 1) userSources
                    in
                    beforeWrapper ++ sourceOverrides ++ wrapper
            in
            case project.baseUserEnv of
                Just baseEnv ->
                    if useResolvedIRPath then
                        Eval.Module.evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw
                            baseEnv
                            newFiles
                            FastDict.empty
                            intercepts
                            Set.empty
                            (FunctionOrValue [] "results")
                            |> resolvedEvalResultToResult

                    else
                        -- Fast path: baseUserEnv has all user modules pre-loaded.
                        -- Only parse sourceOverrides + wrapper (small/new).
                        Eval.Module.evalWithIntercepts baseEnv (sourceOverrides ++ [ wrapperSource ]) intercepts (FunctionOrValue [] "results")
                            |> Result.mapError formatError

                Nothing ->
                    -- Fallback: parse everything from scratch. We don't
                    -- route this through the new path because the new
                    -- entry point assumes the base env already has the
                    -- user modules built in — without a baseUserEnv,
                    -- reusing it would silently miss user declarations.
                    Eval.Module.evalWithIntercepts project.packageEnv allSources intercepts (FunctionOrValue [] "results")
                        |> Result.mapError formatError


formatEvalResult : Result Types.Error Types.Value -> Result String String
formatEvalResult result =
    case result of
        Ok (Types.String s) ->
            Ok s

        Ok _ ->
            Err "ERROR: Expected String result"

        Err err ->
            Err (formatError err)


formatError : Types.Error -> String
formatError err =
    case err of
        Types.ParsingError _ ->
            "ERROR: Parsing error"

        Types.EvalError evalErr ->
            "ERROR: Eval error: "
                ++ evalErrorKindToString evalErr.error
                ++ " [module: "
                ++ String.join "." evalErr.currentModule
                ++ "] [stack: "
                ++ (evalErr.callStack |> List.take 10 |> List.map (\ref -> String.join "." ref.moduleName ++ "." ++ ref.name) |> String.join " <- ")
                ++ "]"



-- HELPERS


{-| Parse "ModuleName.valueName" into its parts.
-}
parseExpression : String -> Maybe ( String, String )
parseExpression expr =
    let
        trimmed =
            String.trim expr
    in
    case String.split "." trimmed |> List.reverse of
        valueName :: modulePartsReversed ->
            if List.isEmpty modulePartsReversed then
                Nothing

            else
                let
                    moduleName =
                        modulePartsReversed |> List.reverse |> String.join "."
                in
                if String.isEmpty valueName || String.isEmpty moduleName then
                    Nothing

                else
                    Just ( moduleName, valueName )

        _ ->
            Nothing


{-| Generate a wrapper module that imports the given modules and evaluates
the expression as `results : String`.
-}
generateWrapper : List String -> String -> String
generateWrapper imports expression =
    let
        importLines : String
        importLines =
            imports
                |> List.map (\m -> "import " ++ m)
                |> String.join "\n"
    in
    "module InterpreterWrapper__ exposing (results)\n\n"
        ++ importLines
        ++ "\n\n\nresults : String\nresults =\n    "
        ++ expression
        ++ "\n"


{-| BFS through the module import graph to find all transitively needed modules.
-}
transitiveModuleDeps : Dict String (Set String) -> Set String -> Set String
transitiveModuleDeps importsGraph rootSet =
    bfsModules importsGraph (Set.toList rootSet) rootSet


bfsModules : Dict String (Set String) -> List String -> Set String -> Set String
bfsModules importsGraph queue visited =
    case queue of
        [] ->
            visited

        current :: rest ->
            let
                directDeps =
                    Dict.get current importsGraph
                        |> Maybe.withDefault Set.empty

                newDeps =
                    Set.diff directDeps visited

                newQueue =
                    rest ++ Set.toList newDeps

                newVisited =
                    Set.union visited newDeps
            in
            bfsModules importsGraph newQueue newVisited


{-| Topologically sort the needed modules using DFS post-order,
returning their sources in dependency order.
-}
topoSortModules : ModuleGraph -> Set String -> List String
topoSortModules graph needed =
    let
        dfs :
            String
            -> { visited : Set String, order : List String }
            -> { visited : Set String, order : List String }
        dfs moduleName acc =
            if Set.member moduleName acc.visited then
                acc

            else
                let
                    withVisited =
                        { acc | visited = Set.insert moduleName acc.visited }

                    deps =
                        Dict.get moduleName graph.imports
                            |> Maybe.withDefault Set.empty
                            |> Set.toList
                            |> List.filter (\dep -> Set.member dep needed)

                    afterDeps =
                        List.foldl dfs withVisited deps
                in
                case Dict.get moduleName graph.moduleToSource of
                    Just src ->
                        { afterDeps | order = afterDeps.order ++ [ src ] }

                    Nothing ->
                        afterDeps

        result =
            Set.toList needed
                |> List.foldl dfs { visited = Set.empty, order = [] }
    in
    result.order


reachableModules : ModuleGraph -> Set String -> Set String
reachableModules graph roots =
    let
        dfs : String -> Set String -> Set String
        dfs moduleName visited =
            if Set.member moduleName visited then
                visited

            else
                let
                    nextVisited =
                        Set.insert moduleName visited

                    deps =
                        Dict.get moduleName graph.imports
                            |> Maybe.withDefault Set.empty
                in
                deps
                    |> Set.toList
                    |> List.foldl dfs nextVisited
    in
    roots
        |> Set.toList
        |> List.foldl dfs Set.empty
