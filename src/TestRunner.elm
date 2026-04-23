module TestRunner exposing
    ( run
    , kernelPackages, patchSource, simpleTestRunnerSource
    )

{-| Run an Elm test suite via the interpreter.

Usage:
    npx elm-pages run src/TestRunner.elm --test tests/MyTests.elm

Or with no args to auto-discover test files.

Add `--load-profile` to print user-normalization rewrite stats after load.

-}

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Env
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Glob as Glob
import BackendTask.Parallel
import BackendTask.Time
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Encode as BE
import Cache
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import DepGraph
import Eval.Module
import FatalError exposing (FatalError)
import InterpreterProject exposing (InterpreterProject)
import Json.Decode
import Json.Encode
import NormalizationFlags
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import ProjectEnvWireCodec
import Set
import TestAnalysis
import TestRunnerCommon
import Time
import Types


type ReportFormat
    = ReportConsole
    | ReportJson


type alias Config =
    { testFile : Maybe String
    , sourceDirs : List String
    , buildDirectory : Path
    , reportFormat : ReportFormat
    , fuzzRuns : Int
    , seed : Int
    , loadProfile : Bool
    , userNormalizationExperiment : Maybe String
    }


type alias UserNormalizationExperiment =
    { label : String
    , flags : NormalizationFlags.NormalizationFlags
    }


{-| Runtime-overridable knobs for the chunking + worker-pool behavior.
Loaded once per `task` invocation by `loadChunkingOverrides`, which
reads three optional env vars and falls back to `defaultChunkingOverrides`
when they're unset:

  - `ELM_BUILD_TARGET_CHUNK_COUNT` — number of chunks per chunked file
  - `ELM_BUILD_PER_CHILD_SPLIT_THRESHOLD` — top-level child count above
    which a file gets chunked at all
  - `ELM_BUILD_POOL_SIZE` — `BackendTask.Parallel.worker` pool size

The defaults reproduce the pre-override behavior so existing callers
see no change. Bench harnesses set the env vars to sweep parameters
without recompiling.

-}
type alias ChunkingOverrides =
    { targetChunkCount : Int
    , perChildSplitThreshold : Int
    , poolSize : Int
    , dispatchBatchSize : Int
    }


{-| Defaults for the chunking knobs.

Chunked dispatch enabled (`perChildSplitThreshold = 10`) — files
whose `all = describe "X" [c1, ..., cN]` has 10+ children get split
into K = `targetChunkCount` chunks dispatched in parallel.

Originally gated off because chunking exposed a runtime-eval cliff
on tight tail-recursive user code (the 1 M-element
`List.Extra.isInfixOf` test in core-extra `tests/ListTests.elm` would
hang >30 s in any chunked-dispatch shape). That cliff was closed by
the resolved-IR evaluator's `dispatchGlobalApplyStep` change to
route `TcoListDrain` calls through OLD eval's `tcoLoopHelp` (see
`elm-interpreter/src/Eval/ResolvedExpression.elm:990`+).

Bench-driven defaults (BUNDLED=1, 5-iter A/B vs the prior no-chunking
+ pool=2 baseline):

  | suite    | no-chunking pool=2 | K=2 P=4 chunked   |
  |----------|--------------------|-------------------|
  | 8-file   | 2301 ± 79 ms       | 2206 ± 32 ms (-4%) |
  | 11-file  | 10619 ± 243 ms     | 9201 ± 145 ms (-13%) |

K=2 P=4 splits the slowest single file (ListTests, ~9 s solo) into
two parallel chunks while keeping enough workers free for the other
files. `dispatchBatchSize = 30` caps `BackendTask.combine` width
under the documented ~40 aggregate cap; `K=2` × 11 chunked files =
22 dispatches, comfortably below.

-}
defaultChunkingOverrides : ChunkingOverrides
defaultChunkingOverrides =
    { targetChunkCount = 2
    , perChildSplitThreshold = 10
    , poolSize = 4
    , dispatchBatchSize = 30
    }


loadChunkingOverrides : BackendTask FatalError ChunkingOverrides
loadChunkingOverrides =
    BackendTask.map4
        (\maybeTcc maybePcst maybePs maybeBs ->
            { targetChunkCount =
                maybeTcc
                    |> Maybe.andThen String.toInt
                    |> Maybe.withDefault defaultChunkingOverrides.targetChunkCount
            , perChildSplitThreshold =
                maybePcst
                    |> Maybe.andThen String.toInt
                    |> Maybe.withDefault defaultChunkingOverrides.perChildSplitThreshold
            , poolSize =
                maybePs
                    |> Maybe.andThen String.toInt
                    |> Maybe.withDefault defaultChunkingOverrides.poolSize
            , dispatchBatchSize =
                maybeBs
                    |> Maybe.andThen String.toInt
                    |> Maybe.withDefault defaultChunkingOverrides.dispatchBatchSize
            }
        )
        (BackendTask.Env.get "ELM_BUILD_TARGET_CHUNK_COUNT")
        (BackendTask.Env.get "ELM_BUILD_PER_CHILD_SPLIT_THRESHOLD")
        (BackendTask.Env.get "ELM_BUILD_POOL_SIZE")
        (BackendTask.Env.get "ELM_BUILD_DISPATCH_BATCH_SIZE")


type alias LoadedProject =
    { project : InterpreterProject
    , profile : Maybe InterpreterProject.LoadProfile
    , experimentLabel : Maybe String
    }


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.optionalKeywordArg "test"
                        |> Option.withDescription "Test file(s), comma-separated (auto-discovered if omitted)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "source-dirs"
                        |> Option.map (Maybe.map (String.split ",") >> Maybe.withDefault [])
                        |> Option.withDescription "Extra source directories (comma-separated)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "build"
                        |> Option.map (Maybe.withDefault ".elm-build" >> Path.path)
                        |> Option.withDescription "Build/cache directory (default: .elm-build)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "report"
                        |> Option.map
                            (Maybe.map parseReportFormat
                                >> Maybe.withDefault ReportConsole
                            )
                        |> Option.withDescription "Output format: console or json (default: console)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "fuzz"
                        |> Option.map
                            (\maybeFuzz ->
                                maybeFuzz
                                    |> Maybe.andThen String.toInt
                                    |> Maybe.map (max 1)
                                    |> Maybe.withDefault 1
                            )
                        |> Option.withDescription "Fuzz runs per fuzz test (default: 1)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "seed"
                        |> Option.map
                            (\maybeSeed ->
                                maybeSeed
                                    |> Maybe.andThen String.toInt
                                    |> Maybe.withDefault 42
                            )
                        |> Option.withDescription "Random seed for fuzz tests (default: 42)"
                    )
                |> OptionsParser.with
                    (Option.flag "load-profile"
                        |> Option.withDescription "Print load-time user normalization and rewrite counters"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "user-normalization-experiment"
                        |> Option.withDescription "Opt-in user-source normalization preset (for example: none, default, inline-5, precomputed-refs, list-fusion-off)"
                    )
            )


run : Script
run =
    Script.withCliOptions programConfig (task >> BackendTask.quiet)


task : Config -> BackendTask FatalError ()
task config =
    case resolveUserNormalizationExperiment config.userNormalizationExperiment of
        Err message ->
            BackendTask.fail (FatalError.fromString message)

        Ok maybeExperiment ->
            Do.do (logIfConsole config (Ansi.Color.fontColor Ansi.Color.brightBlue "Running tests via interpreter")) <| \_ ->
            Do.do ensureDeps <| \_ ->
            let
                sourceDirectories =
                    "src" :: config.sourceDirs

                allDirectories =
                    "tests" :: sourceDirectories
            in
            Do.do (resolveTestFiles config) <| \testFiles ->
            Do.do (resolveTestModuleNames testFiles) <| \testModuleNames ->
            Do.do loadChunkingOverrides <| \overrides ->
            Do.do (loadProject config maybeExperiment allDirectories testModuleNames) <| \loaded ->
            Do.do
                (case loaded.profile of
                    Just profile ->
                        if config.loadProfile then
                            logIfConsole config (formatLoadProfile loaded.experimentLabel profile)

                        else
                            BackendTask.succeed ()

                    Nothing ->
                        BackendTask.succeed ()
                )
            <| \_ ->
            Do.do (logIfConsole config ("Found " ++ String.fromInt (List.length testFiles) ++ " test file(s)")) <| \_ ->
            Do.do BackendTask.Time.now <| \startTime ->
            let
                baseUserEnvWireBytes : Bytes
                baseUserEnvWireBytes =
                    case InterpreterProject.getBaseUserEnv loaded.project of
                        Just env ->
                            env
                                |> Eval.Module.toWireFields
                                |> ProjectEnvWireCodec.encodeWireFields

                        Nothing ->
                            -- Empty Bytes signals "no env shipped" — worker
                            -- falls back to loadWithPreBuiltGraphs.
                            BE.encode (BE.sequence [])
            in
            Do.do
                (BackendTask.Parallel.worker
                    { workerModule = "TestFileWorker"
                    , shared =
                        TestRunnerCommon.encodeWorkerShared
                            { projectDir = "."
                            , sourceDirectories = allDirectories
                            , testModuleNames = testModuleNames
                            , depGraph = InterpreterProject.getDepGraph loaded.project
                            , baseUserEnvWireBytes = baseUserEnvWireBytes
                            }

                    -- Pool=2 is the post-step-9 default (see
                    -- `defaultChunkingOverrides`). Override at run time
                    -- via `ELM_BUILD_POOL_SIZE` for benchmark sweeps.
                    -- Empirical pre-override numbers (5 runs each):
                    --   pool=1 11-file 11026.7 ms   8-file 3498.8 ms
                    --   pool=2 11-file 10546.9 ms   8-file 3382.1 ms  ← best
                    --   pool=3 11-file 10606.4 ms
                    --   pool=4 11-file 10710.8 ms   8-file 3455.5 ms
                    , poolSize = Just overrides.poolSize
                    }
                )
            <| \worker ->
            Do.do
                (testFiles
                    |> List.map (\testFile -> runTestFileViaWorker config overrides loaded.project worker testFile)
                    |> BackendTask.combine
                )
            <| \allResults ->
            Do.do BackendTask.Time.now <| \endTime ->
            let
                summary =
                    { durationMs = Time.posixToMillis endTime - Time.posixToMillis startTime
                    , seed = config.seed
                    , fuzzRuns = config.fuzzRuns
                    , totalPassed = List.sum (List.map .passed allResults)
                    , totalFailed = List.sum (List.map .failed allResults)
                    , totalSkipped = List.sum (List.map .skipped allResults)
                    , files = allResults
                    }
            in
            Do.do (emitRunSummary config summary) <| \_ ->
            if summary.totalFailed > 0 then
                BackendTask.fail
                    (FatalError.build
                        { title = "Tests Failed"
                        , body = String.fromInt summary.totalFailed ++ " tests failed"
                        }
                    )

            else
                Do.noop


type alias RunSummary =
    { durationMs : Int
    , seed : Int
    , fuzzRuns : Int
    , totalPassed : Int
    , totalFailed : Int
    , totalSkipped : Int
    , files : List TestFileResult
    }


loadProject : Config -> Maybe UserNormalizationExperiment -> List String -> List String -> BackendTask FatalError LoadedProject
loadProject config maybeExperiment allDirectories testModuleNames =
    let
        baseLoadConfig =
            { projectDir = Path.path "."
            , skipPackages = kernelPackages
            , patchSource = patchSource
            , patchUserSource = \_ source -> source
            , extraSourceFiles = []
            , extraReachableImports = [ "Test", "Fuzz", "Expect", "Test.Runner" ]
            , sourceDirectories = Just allDirectories
            , normalizationRoots = Just testModuleNames
            }

        profileLoadConfig =
            { projectDir = Path.path "."
            , skipPackages = kernelPackages
            , patchSource = patchSource
            , patchUserSource = \_ source -> source
            , extraSourceFiles = []
            , extraReachableImports = [ "Test", "Fuzz", "Expect", "Test.Runner" ]
            , sourceDirectories = Just allDirectories
            , normalizationRoots = Just testModuleNames
            , packageParseCacheDir = Just ".elm-build"
            , preBuiltDepGraph = Nothing
            , preBuiltModuleGraph = Nothing
            }

        loadTask : BackendTask FatalError LoadedProject
        loadTask =
            case ( config.loadProfile, maybeExperiment ) of
                ( True, Just experiment ) ->
                    InterpreterProject.loadWithProfileUserNormalizationFlags experiment.flags profileLoadConfig
                        |> BackendTask.map
                            (\loaded ->
                                { project = loaded.project
                                , profile = Just loaded.profile
                                , experimentLabel = Just experiment.label
                                }
                            )

                ( True, Nothing ) ->
                    InterpreterProject.loadWithProfile profileLoadConfig
                        |> BackendTask.map
                            (\loaded ->
                                { project = loaded.project
                                , profile = Just loaded.profile
                                , experimentLabel = Nothing
                                }
                            )

                ( False, Just experiment ) ->
                    InterpreterProject.loadWithUserNormalizationFlags experiment.flags baseLoadConfig
                        |> BackendTask.map
                            (\project ->
                                { project = project
                                , profile = Nothing
                                , experimentLabel = Just experiment.label
                                }
                            )

                ( False, Nothing ) ->
                    InterpreterProject.loadWith baseLoadConfig
                        |> BackendTask.map
                            (\project ->
                                { project = project
                                , profile = Nothing
                                , experimentLabel = Nothing
                                }
                            )
    in
    case config.reportFormat of
        ReportConsole ->
            BackendTask.Extra.timed "Loading project" "Loaded project" loadTask

        ReportJson ->
            loadTask


logIfConsole : Config -> String -> BackendTask FatalError ()
logIfConsole config message =
    case config.reportFormat of
        ReportConsole ->
            Script.log message

        ReportJson ->
            BackendTask.succeed ()


emitRunSummary : Config -> RunSummary -> BackendTask FatalError ()
emitRunSummary config summary =
    case config.reportFormat of
        ReportConsole ->
            let
                summaryColor =
                    if summary.totalFailed == 0 && summary.totalSkipped == 0 then
                        Ansi.Color.fontColor Ansi.Color.brightGreen

                    else if summary.totalFailed > 0 then
                        Ansi.Color.fontColor Ansi.Color.brightRed

                    else
                        Ansi.Color.fontColor Ansi.Color.yellow
            in
            Script.log
                (summaryColor
                    ("\nDuration: "
                        ++ String.fromInt summary.durationMs
                        ++ "ms\nPassed:   "
                        ++ String.fromInt summary.totalPassed
                        ++ "\nFailed:   "
                        ++ String.fromInt summary.totalFailed
                        ++ (if summary.totalSkipped > 0 then
                                "\nSkipped:  " ++ String.fromInt summary.totalSkipped ++ " (interpreter limitations)"

                            else
                                ""
                           )
                    )
                )

        ReportJson ->
            encodeRunSummary summary
                |> Json.Encode.encode 0
                |> Script.log


encodeRunSummary : RunSummary -> Json.Encode.Value
encodeRunSummary summary =
    Json.Encode.object
        [ ( "runner", Json.Encode.string "elm-build-test-runner" )
        , ( "complete", Json.Encode.bool (summary.totalSkipped == 0) )
        , ( "durationMs", Json.Encode.int summary.durationMs )
        , ( "seed", Json.Encode.int summary.seed )
        , ( "fuzzRuns", Json.Encode.int summary.fuzzRuns )
        , ( "totalPassed", Json.Encode.int summary.totalPassed )
        , ( "totalFailed", Json.Encode.int summary.totalFailed )
        , ( "totalSkipped", Json.Encode.int summary.totalSkipped )
        , ( "files", Json.Encode.list encodeTestFileResult summary.files )
        ]


type alias TestCaseResult =
    { label : String
    , passed : Bool
    , message : String
    }


encodeTestCaseResult : TestCaseResult -> Json.Encode.Value
encodeTestCaseResult result =
    Json.Encode.object
        [ ( "label", Json.Encode.string result.label )
        , ( "status"
          , Json.Encode.string
                (if result.passed then
                    "pass"

                 else
                    "fail"
                )
          )
        , ( "message", Json.Encode.string result.message )
        ]


formatLoadProfile : Maybe String -> InterpreterProject.LoadProfile -> String
formatLoadProfile maybeExperimentLabel profile =
    let
        userNormStats =
            profile.userNormDependencySummaryStats

        experimentLabel =
            Maybe.withDefault "current-default" maybeExperimentLabel

        rejectSummary =
            formatNonZeroCounters
                [ ( "pattern", userNormStats.inlineRejectedPattern )
                , ( "arity", userNormStats.inlineRejectedArity )
                , ( "self_call", userNormStats.inlineRejectedSelfCall )
                , ( "body_too_large", userNormStats.inlineRejectedBodyTooLarge )
                , ( "unsafe", userNormStats.inlineRejectedUnsafe )
                , ( "internal_helper", userNormStats.inlineRejectedInternalHelper )
                ]

        payoffSummary =
            formatNonZeroCounters
                [ ( "changed", userNormStats.inlinePayoffChanged )
                , ( "inline", userNormStats.inlinePayoffInline )
                , ( "constant_fold", userNormStats.inlinePayoffConstantFold )
                , ( "precomputed_ref", userNormStats.inlinePayoffPrecomputedRef )
                ]

        shadowRejectSummary =
            formatNonZeroCounters
                [ ( "collection", userNormStats.inlineShadowRejectCollection )
                , ( "growth0", userNormStats.inlineShadowRejectGrowth0 )
                , ( "growth1", userNormStats.inlineShadowRejectGrowth1 )
                ]

        sampleLines =
            userNormStats.rejectSamples
                |> List.take 6
                |> List.indexedMap
                    (\index sample ->
                        "  reject_sample_" ++ String.fromInt (index + 1) ++ "=" ++ sample
                    )
    in
    String.join "\n"
        ([ "Load profile:"
         , "  user_norm_experiment=" ++ experimentLabel
         , "  ─ phase timings ─"
         , "  resolve_source_directories_ms=" ++ String.fromInt profile.resolveSourceDirectoriesMs
         , "  load_package_sources_ms=" ++ String.fromInt profile.loadPackageSourcesMs
         , "  load_package_summary_cache_ms=" ++ String.fromInt profile.loadPackageSummaryCacheMs
         , "  decode_package_summary_cache_ms=" ++ String.fromInt profile.decodePackageSummaryCacheMs
         , "  validate_package_summary_cache_ms=" ++ String.fromInt profile.validatePackageSummaryCacheMs
         , "  write_package_summary_cache_ms=" ++ String.fromInt profile.writePackageSummaryCacheMs
         , "  glob_user_sources_ms=" ++ String.fromInt profile.globUserSourcesMs
         , "  read_user_sources_ms=" ++ String.fromInt profile.readUserSourcesMs
         , "  read_extra_sources_ms=" ++ String.fromInt profile.readExtraSourcesMs
         , "  build_graph_ms=" ++ String.fromInt profile.buildGraphMs
         , "  parse_package_sources_ms=" ++ String.fromInt profile.parsePackageSourcesMs
         , "  build_package_summaries_from_parsed_ms=" ++ String.fromInt profile.buildPackageSummariesFromParsedMs
         , "  build_base_user_env_ms=" ++ String.fromInt profile.buildBaseUserEnvMs
         , "  ─ counters ─"
         , "  user_norm_modules_planned=" ++ String.fromInt profile.userNormModulesPlanned
            ++ " target_functions="
            ++ String.fromInt profile.userNormTargetFunctions
         , "  user_norm_cache_hit_modules=" ++ String.fromInt profile.userNormCacheHitModules
            ++ " cache_miss_modules="
            ++ String.fromInt profile.userNormCacheMissModules
            ++ " cache_extended_modules="
            ++ String.fromInt profile.userNormCacheExtendedModules
         , "  user_norm_functions_visited=" ++ String.fromInt userNormStats.functionsVisited
            ++ " rewritten_functions="
            ++ String.fromInt profile.userNormRewrittenFunctions
            ++ " precomputed_values="
            ++ String.fromInt profile.userNormPrecomputedValues
         , "  inline_candidates=" ++ String.fromInt userNormStats.inlineCandidates
            ++ " inline_successes="
            ++ String.fromInt userNormStats.inlineSuccesses
         , "  inline_rejects=" ++ rejectSummary
         , "  inline_payoff=" ++ payoffSummary
         , "  inline_shadow_rejects=" ++ shadowRejectSummary
         , "  list_fusion_changes=" ++ String.fromInt userNormStats.listFusionChanges
            ++ " pipeline_normalizations="
            ++ String.fromInt userNormStats.listFusionPipelineNormalizations
            ++ " head_flatten_rewrites="
            ++ String.fromInt userNormStats.listFusionHeadFlattenRewrites
            ++ " rule_rewrites="
            ++ String.fromInt userNormStats.listFusionRuleRewrites
         , "  constant_folds=" ++ String.fromInt userNormStats.constantFolds
            ++ " precomputed_ref_substitutions="
            ++ String.fromInt userNormStats.precomputedRefSubstitutions
         ]
            ++ sampleLines
        )


resolveUserNormalizationExperiment : Maybe String -> Result String (Maybe UserNormalizationExperiment)
resolveUserNormalizationExperiment maybeExperimentName =
    case maybeExperimentName of
        Nothing ->
            Ok Nothing

        Just experimentName ->
            parseUserNormalizationExperiment experimentName
                |> Maybe.map Just
                |> Result.fromMaybe
                    ("Unknown user normalization experiment `"
                        ++ experimentName
                        ++ "`. Expected one of: "
                        ++ String.join ", " knownUserNormalizationExperiments
                    )


parseUserNormalizationExperiment : String -> Maybe UserNormalizationExperiment
parseUserNormalizationExperiment rawName =
    let
        name =
            String.toLower rawName

        defaultFlags =
            NormalizationFlags.default

        mk label flags =
            { label = label, flags = flags }
    in
    case name of
        "default" ->
            Just (mk "default" NormalizationFlags.default)

        "experimental" ->
            Just (mk "experimental" NormalizationFlags.experimental)

        "none" ->
            Just (mk "none" NormalizationFlags.none)

        "precomputed-refs" ->
            Just
                (mk "precomputed-refs"
                    { defaultFlags
                        | inlinePrecomputedRefs = True
                    }
                )

        "inline-5" ->
            Just
                (mk "inline-5"
                    { defaultFlags
                        | inlineFunctions = True
                        , inlineFunctionMaxSize = 5
                    }
                )

        "inline-20" ->
            Just
                (mk "inline-20"
                    { defaultFlags
                        | inlineFunctions = True
                        , inlineFunctionMaxSize = 20
                    }
                )

        "inline-5-precomputed-refs" ->
            Just
                (mk "inline-5-precomputed-refs"
                    { defaultFlags
                        | inlinePrecomputedRefs = True
                        , inlineFunctions = True
                        , inlineFunctionMaxSize = 5
                    }
                )

        "inline-20-precomputed-refs" ->
            Just
                (mk "inline-20-precomputed-refs"
                    { defaultFlags
                        | inlinePrecomputedRefs = True
                        , inlineFunctions = True
                        , inlineFunctionMaxSize = 20
                    }
                )

        "fixpoint-off" ->
            Just
                (mk "fixpoint-off"
                    { defaultFlags
                        | runFixpoint = False
                        , fixpointPasses = 0
                        , tryNormalizeMaxSteps = 0
                    }
                )

        "list-fusion-off" ->
            Just
                (mk "list-fusion-off"
                    { defaultFlags
                        | runListFusion = False
                        , fuseListMaps = False
                    }
                )

        "all" ->
            Just (mk "all" NormalizationFlags.all)

        _ ->
            Nothing


knownUserNormalizationExperiments : List String
knownUserNormalizationExperiments =
    [ "default"
    , "experimental"
    , "none"
    , "precomputed-refs"
    , "inline-5"
    , "inline-20"
    , "inline-5-precomputed-refs"
    , "inline-20-precomputed-refs"
    , "fixpoint-off"
    , "list-fusion-off"
    , "all"
    ]


formatNonZeroCounters : List ( String, Int ) -> String
formatNonZeroCounters counters =
    let
        nonZeroCounters =
            counters
                |> List.filter (\( _, count ) -> count > 0)
                |> List.map (\( label, count ) -> label ++ "=" ++ String.fromInt count)
    in
    if List.isEmpty nonZeroCounters then
        "none"

    else
        String.join ", " nonZeroCounters


type alias TestFileResult =
    { file : String
    , moduleName : String
    , status : String
    , message : String
    , passed : Int
    , failed : Int
    , skipped : Int
    , tests : List TestCaseResult
    }


encodeTestFileResult : TestFileResult -> Json.Encode.Value
encodeTestFileResult result =
    Json.Encode.object
        [ ( "file", Json.Encode.string result.file )
        , ( "moduleName", Json.Encode.string result.moduleName )
        , ( "status", Json.Encode.string result.status )
        , ( "message", Json.Encode.string result.message )
        , ( "passed", Json.Encode.int result.passed )
        , ( "failed", Json.Encode.int result.failed )
        , ( "skipped", Json.Encode.int result.skipped )
        , ( "tests", Json.Encode.list encodeTestCaseResult result.tests )
        ]


type alias ParsedTestOutput =
    { passed : Int
    , failed : Int
    , total : Int
    , tests : List TestCaseResult
    }


parseTestOutput : String -> Maybe ParsedTestOutput
parseTestOutput output =
    case String.lines output |> List.head |> Maybe.withDefault "" |> String.split "," of
        [ passStr, failStr, totalStr ] ->
            case ( String.toInt passStr, String.toInt failStr, String.toInt totalStr ) of
                ( Just passed, Just failed, Just total ) ->
                    Just
                        { passed = passed
                        , failed = failed
                        , total = total
                        , tests = parseTestCaseLines (String.lines output |> List.drop 1)
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


parseTestCaseLines : List String -> List TestCaseResult
parseTestCaseLines lines =
    lines
        |> List.filterMap parseTestCaseLine


prefixTestModuleLabel : String -> TestCaseResult -> TestCaseResult
prefixTestModuleLabel testModuleName result =
    let
        expectedPrefix =
            testModuleName ++ " > "
    in
    if result.label == testModuleName || String.startsWith expectedPrefix result.label then
        result

    else
        { result | label = expectedPrefix ++ result.label }


parseTestCaseLine : String -> Maybe TestCaseResult
parseTestCaseLine line =
    if String.startsWith "PASS:" line then
        Just
            { label = String.dropLeft 5 line
            , passed = True
            , message = ""
            }

    else if String.startsWith "FAIL:" line then
        let
            body =
                String.dropLeft 5 line
        in
        case String.indexes " | " body |> List.head of
            Just separatorIndex ->
                Just
                    { label = String.left separatorIndex body
                    , passed = False
                    , message = String.dropLeft (separatorIndex + 3) body
                    }

            Nothing ->
                Just
                    { label = body
                    , passed = False
                    , message = ""
                    }

    else
        Nothing


runTestFile : Config -> InterpreterProject -> String -> BackendTask FatalError TestFileResult
runTestFile config project testFile =
    Do.allowFatal (File.rawFile testFile) <| \testSource ->
    let
        testModuleName =
            DepGraph.parseModuleName testSource |> Maybe.withDefault "Tests"

        staticTestValues =
            TestAnalysis.discoverTestValues testSource

        probeTestValues () =
            let
                candidateNames =
                    TestAnalysis.getCandidateNames testSource

                packageEnv =
                    InterpreterProject.getPackageEnv project

                probeSources =
                    let
                        { userSources } =
                            InterpreterProject.prepareEvalSources project
                                { imports = [ "SimpleTestRunner", testModuleName ]
                                , expression = "\"probe\""
                                }
                    in
                    simpleTestRunnerSource config :: userSources

                probeResults =
                    candidateNames
                        |> List.map
                            (\name ->
                                ( name, TestAnalysis.probeCandidate packageEnv testModuleName name probeSources )
                            )
            in
            { testValues =
                probeResults
                    |> List.filterMap
                        (\( name, result ) ->
                            case result of
                                Ok _ ->
                                    Just name

                                Err _ ->
                                    Nothing
                        )
            , rejections =
                probeResults
                    |> List.filterMap
                        (\( name, result ) ->
                            case result of
                                Err reason ->
                                    Just (name ++ ": " ++ reason)

                                Ok _ ->
                                    Nothing
                        )
                    |> String.join "; "
            , candidateCount = List.length candidateNames
            }

        skippedResult reason skippedCount =
            { file = testFile
            , moduleName = testModuleName
            , status = "skipped"
            , message = reason
            , passed = 0
            , failed = 0
            , skipped = skippedCount
            , tests = []
            }

        { testValues, rejections, candidateCount } =
            if List.isEmpty staticTestValues then
                probeTestValues ()

            else
                { testValues = staticTestValues
                , rejections = ""
                , candidateCount = List.length staticTestValues
                }
    in
    if List.isEmpty testValues then
        Do.do (logIfConsole config (Ansi.Color.fontColor Ansi.Color.yellow ("  ⊘ " ++ testModuleName ++ " (skipped: " ++ rejections ++ ")"))) <| \_ ->
        BackendTask.succeed (skippedResult rejections (max 1 candidateCount))

    else
        let
            suiteExpr =
                case testValues of
                    [ single ] ->
                        testModuleName ++ "." ++ single

                    multiple ->
                        "Test.describe \"" ++ testModuleName ++ "\" [" ++ String.join ", " (List.map (\v -> testModuleName ++ "." ++ v) multiple) ++ "]"

            evalExpression =
                "SimpleTestRunner.runToString (" ++ suiteExpr ++ ")"
        in
        Do.do
            (Cache.run { jobs = Nothing } config.buildDirectory
                (InterpreterProject.evalWithFileOverrides project
                    { imports = [ "SimpleTestRunner", "Test", testModuleName ]
                    , expression = evalExpression
                    , sourceOverrides = [ simpleTestRunnerSource config ]
                    , fileOverrides = []
                    }
                    Cache.succeed
                )
            )
        <| \cacheResult ->
        Do.allowFatal (File.rawFile (Path.toString cacheResult.output)) <| \output ->
        if String.startsWith "ERROR:" output then
            let
                message =
                    String.trim output
            in
            Do.do (logIfConsole config (Ansi.Color.fontColor Ansi.Color.yellow ("  ⊘ " ++ testModuleName ++ " (error: " ++ String.left 500 message ++ ")"))) <| \_ ->
            BackendTask.succeed (skippedResult message 1)

        else
            case parseTestOutput output of
                Just parsedOutput ->
                    let
                        normalizedTests =
                            parsedOutput.tests
                                |> List.map (prefixTestModuleLabel testModuleName)

                        failures =
                            normalizedTests
                                |> List.filter (not << .passed)
                    in
                    Do.do
                        (if parsedOutput.failed == 0 then
                            logIfConsole config (Ansi.Color.fontColor Ansi.Color.green ("  ✓ " ++ testModuleName ++ " (" ++ String.fromInt parsedOutput.passed ++ " passed)"))

                         else
                            Do.do
                                (logIfConsole config (Ansi.Color.fontColor Ansi.Color.red ("  ✗ " ++ testModuleName ++ " (" ++ String.fromInt parsedOutput.failed ++ " failed, " ++ String.fromInt parsedOutput.passed ++ " passed)")))
                            <| \_ ->
                            Do.each failures
                                (\failure ->
                                    logIfConsole config
                                        (Ansi.Color.fontColor Ansi.Color.red
                                            ("      FAIL:"
                                                ++ failure.label
                                                ++ (if String.isEmpty failure.message then
                                                        ""

                                                    else
                                                        " | " ++ failure.message
                                                   )
                                            )
                                        )
                                )
                                (\_ -> BackendTask.succeed ())
                        )
                    <| \_ ->
                    BackendTask.succeed
                        { file = testFile
                        , moduleName = testModuleName
                        , status =
                            if parsedOutput.failed == 0 then
                                "passed"

                            else
                                "failed"
                        , message = ""
                        , passed = parsedOutput.passed
                        , failed = parsedOutput.failed
                        , skipped = 0
                        , tests = normalizedTests
                        }

                Nothing ->
                    Do.do (logIfConsole config (Ansi.Color.fontColor Ansi.Color.yellow ("  ⊘ " ++ testModuleName ++ " (unexpected output)"))) <| \_ ->
                    BackendTask.succeed (skippedResult "unexpected output" 1)


{-| Worker variant of `runTestFile`: same orchestration on the main
thread (file read, test discovery, expression construction, output
parsing, logging), but the heavy interpreter eval is dispatched to a
free thread in the `BackendTask.Parallel.worker` pool — only on cache
miss.

`InterpreterProject.evalCachedViaTask` wraps the per-file dispatch in
the same `Cache.computeAsync` semantic-hash key that
`evalWithFileOverrides` uses, so warm reruns hit the on-disk cache and
short-circuit before the worker is touched. Cold runs miss, dispatch
to a worker, and the result gets written back to cache for next time.

The cache namespace is shared with `evalWithFileOverrides` (same
`["interpret-with-file-overrides", interpreterResultCacheVersion]`
label), so cache files are interchangeable between the two paths.

-}
runTestFileViaWorker : Config -> ChunkingOverrides -> InterpreterProject -> BackendTask.Parallel.Worker -> String -> BackendTask FatalError TestFileResult
runTestFileViaWorker config overrides project worker testFile =
    Do.allowFatal (File.rawFile testFile) <| \testSource ->
    let
        testModuleName : String
        testModuleName =
            DepGraph.parseModuleName testSource |> Maybe.withDefault "Tests"

        staticTestValues : List String
        staticTestValues =
            TestAnalysis.discoverTestValues testSource

        probeTestValues : () -> { testValues : List String, rejections : String, candidateCount : Int }
        probeTestValues () =
            let
                candidateNames =
                    TestAnalysis.getCandidateNames testSource

                packageEnv =
                    InterpreterProject.getPackageEnv project

                probeSources =
                    let
                        { userSources } =
                            InterpreterProject.prepareEvalSources project
                                { imports = [ "SimpleTestRunner", testModuleName ]
                                , expression = "\"probe\""
                                }
                    in
                    simpleTestRunnerSource config :: userSources

                probeResults =
                    candidateNames
                        |> List.map
                            (\name ->
                                ( name, TestAnalysis.probeCandidate packageEnv testModuleName name probeSources )
                            )
            in
            { testValues =
                probeResults
                    |> List.filterMap
                        (\( name, result ) ->
                            case result of
                                Ok _ ->
                                    Just name

                                Err _ ->
                                    Nothing
                        )
            , rejections =
                probeResults
                    |> List.filterMap
                        (\( name, result ) ->
                            case result of
                                Err reason ->
                                    Just (name ++ ": " ++ reason)

                                Ok _ ->
                                    Nothing
                        )
                    |> String.join "; "
            , candidateCount = List.length candidateNames
            }

        skippedResult reason skippedCount =
            { file = testFile
            , moduleName = testModuleName
            , status = "skipped"
            , message = reason
            , passed = 0
            , failed = 0
            , skipped = skippedCount
            , tests = []
            }

        { testValues, rejections, candidateCount } =
            if List.isEmpty staticTestValues then
                probeTestValues ()

            else
                { testValues = staticTestValues
                , rejections = ""
                , candidateCount = List.length staticTestValues
                }
    in
    if List.isEmpty testValues then
        Do.do (logIfConsole config (Ansi.Color.fontColor Ansi.Color.yellow ("  ⊘ " ++ testModuleName ++ " (skipped: " ++ rejections ++ ")"))) <| \_ ->
        BackendTask.succeed (skippedResult rejections (max 1 candidateCount))

    else
        let
            childExpressions : List String
            childExpressions =
                planChildExpressions overrides testModuleName testSource testValues

            evalImports : List String
            evalImports =
                -- Expose `Test` (describe / test / fuzz / …) and `Expect`
                -- so per-child split expressions — which use those names
                -- unqualified the same way the test source does — resolve.
                [ "SimpleTestRunner"
                , "Test exposing (Test, describe, test, fuzz, fuzz2, fuzz3, fuzz4, fuzz5, only, skip, todo, concat)"
                , "Expect"
                , "Fuzz"
                , testModuleName
                ]

            evalSourceOverrides : List String
            evalSourceOverrides =
                [ simpleTestRunnerSource config ]

            dispatchOne : Cache.HashSet -> String -> BackendTask FatalError String
            dispatchOne existing expr =
                let
                    evalExpression =
                        "SimpleTestRunner.runToString (" ++ expr ++ ")"

                    taskInputBytes : Bytes
                    taskInputBytes =
                        encodeWorkerTaskInput
                            { imports = evalImports
                            , expression = evalExpression
                            , sourceOverrides = evalSourceOverrides
                            }
                in
                Cache.runWith { jobs = Nothing, existing = existing }
                    config.buildDirectory
                    (InterpreterProject.evalCachedViaTask project
                        { imports = evalImports
                        , expression = evalExpression
                        , sourceOverrides = evalSourceOverrides
                        , compute =
                            BackendTask.Parallel.runOn worker taskInputBytes workerTaskOutputDecoder
                        }
                    )
                    |> BackendTask.andThen
                        (\cacheResult ->
                            File.rawFile (Path.toString cacheResult.output)
                                |> BackendTask.allowFatal
                        )
        in
        Do.do (Cache.listExisting config.buildDirectory) <| \existing ->
        Do.do
            (childExpressions
                |> List.map (dispatchOne existing)
                |> batchedCombine overrides.dispatchBatchSize
            )
        <| \perChildOutputs ->
        let
            firstError : Maybe String
            firstError =
                perChildOutputs
                    |> List.filter (String.startsWith "ERROR:")
                    |> List.head
        in
        case firstError of
            Just errorOutput ->
                let
                    message =
                        String.trim errorOutput
                in
                Do.do (logIfConsole config (Ansi.Color.fontColor Ansi.Color.yellow ("  ⊘ " ++ testModuleName ++ " (error: " ++ String.left 500 message ++ ")"))) <| \_ ->
                BackendTask.succeed (skippedResult message 1)

            Nothing ->
                let
                    parsedOutputs : List ParsedTestOutput
                    parsedOutputs =
                        perChildOutputs
                            |> List.filterMap parseTestOutput

                    aggregated : ParsedTestOutput
                    aggregated =
                        { passed = List.sum (List.map .passed parsedOutputs)
                        , failed = List.sum (List.map .failed parsedOutputs)
                        , total = List.sum (List.map .total parsedOutputs)
                        , tests = List.concatMap .tests parsedOutputs
                        }
                in
                if List.length parsedOutputs /= List.length perChildOutputs then
                    Do.do (logIfConsole config (Ansi.Color.fontColor Ansi.Color.yellow ("  ⊘ " ++ testModuleName ++ " (unexpected output)"))) <| \_ ->
                    BackendTask.succeed (skippedResult "unexpected output" 1)

                else
                    let
                        normalizedTests =
                            aggregated.tests
                                |> List.map (prefixTestModuleLabel testModuleName)

                        failures =
                            normalizedTests
                                |> List.filter (not << .passed)
                    in
                    Do.do
                        (if aggregated.failed == 0 then
                            logIfConsole config (Ansi.Color.fontColor Ansi.Color.green ("  ✓ " ++ testModuleName ++ " (" ++ String.fromInt aggregated.passed ++ " passed)"))

                         else
                            Do.do
                                (logIfConsole config (Ansi.Color.fontColor Ansi.Color.red ("  ✗ " ++ testModuleName ++ " (" ++ String.fromInt aggregated.failed ++ " failed, " ++ String.fromInt aggregated.passed ++ " passed)")))
                            <| \_ ->
                            Do.each failures
                                (\failure ->
                                    logIfConsole config
                                        (Ansi.Color.fontColor Ansi.Color.red
                                            ("      FAIL:"
                                                ++ failure.label
                                                ++ (if String.isEmpty failure.message then
                                                        ""

                                                    else
                                                        " | " ++ failure.message
                                                   )
                                            )
                                        )
                                )
                                (\_ -> BackendTask.succeed ())
                        )
                    <| \_ ->
                    BackendTask.succeed
                        { file = testFile
                        , moduleName = testModuleName
                        , status =
                            if aggregated.failed == 0 then
                                "passed"

                            else
                                "failed"
                        , message = ""
                        , passed = aggregated.passed
                        , failed = aggregated.failed
                        , skipped = 0
                        , tests = normalizedTests
                        }


{-| Plan the list of eval-suite expressions to dispatch for a single
test file.

Default: one expression (the file's whole suite) — same behavior as the
pre-split implementation. Worker pool gets one big task per file.

When a file has exactly one top-level `Test` value (the common
`all : Test\nall = describe "X" [...]` shape) AND the top-level
`describe`'s child list has at least `perChildSplitThreshold` items,
we split: each child becomes its own dispatch. This breaks the Amdahl
ceiling on cold runs of single-file-dominated suites (e.g. core-extra's
ListTests has ~70 child describes under one `all` — splitting takes
the cold-run wall from ~9 s solo down to roughly `9 s / N_workers`).

-}
planChildExpressions : ChunkingOverrides -> String -> String -> List String -> List String
planChildExpressions overrides testModuleName testSource testValues =
    case testValues of
        [ single ] ->
            case TestAnalysis.extractDescribeChildren single testSource of
                Just children ->
                    if List.length children >= overrides.perChildSplitThreshold then
                        chunkChildExpressions overrides testModuleName children

                    else
                        [ testModuleName ++ "." ++ single ]

                Nothing ->
                    [ testModuleName ++ "." ++ single ]

        multiple ->
            [ "Test.describe \"" ++ testModuleName ++ "\" [" ++ String.join ", " (List.map (\v -> testModuleName ++ "." ++ v) multiple) ++ "]" ]


{-| Group a file's top-level `describe` children into a small number of
synthetic `describe` chunks, one per dispatch.

Per-child dispatch (one `runOn` per child) hangs the runtime at N ≥ 60
children — see `perChildSplitThreshold` doc + `.scratch/parallel-ceiling.md`.
Chunking caps in-flight Cache+dispatch chains at `targetChunkCount` so
the runtime stays in the known-good range while still spreading the
work across the worker pool.

For ListTests (77 children, pool=4): chunks into 4 synthetic describes
of ~20 children each. Each chunk dispatches as one `runOn`. Wall
becomes `max(per-chunk eval) ≈ N_children/N_chunks × per-child-time`
— for ListTests, ~20 × 110 ms = ~2 s vs the unsplit 9 s solo eval.

-}
chunkChildExpressions : ChunkingOverrides -> String -> List String -> List String
chunkChildExpressions overrides testModuleName children =
    let
        n : Int
        n =
            List.length children

        chunkSize : Int
        chunkSize =
            max 1 (ceiling (toFloat n / toFloat overrides.targetChunkCount))
    in
    children
        |> chunkList chunkSize
        |> List.indexedMap
            (\i chunk ->
                "Test.describe \""
                    ++ testModuleName
                    ++ "/chunk-"
                    ++ String.fromInt i
                    ++ "\" ["
                    ++ String.join ", " chunk
                    ++ "]"
            )


{-| Greedy fixed-size chunking of a list. `chunkList 3 [1..10]` →
`[[1,2,3], [4,5,6], [7,8,9], [10]]`. Tail-recursive via List.foldl on
chunk indices; doesn't preserve original recursion stack like a naïve
`take/drop` recursion would for big lists.
-}
chunkList : Int -> List a -> List (List a)
chunkList n xs =
    if List.isEmpty xs then
        []

    else
        List.take n xs :: chunkList n (List.drop n xs)


{-| `BackendTask.combine` with bounded concurrency: process the input
in groups of at most `batchSize`, completing each group before starting
the next. Preserves output order.

The aggregate-dispatch cap measured in 2026-04-22 sits between 60 and
65 single-child dispatches across `BackendTask.combine` (a chunking
sweep on `tests/ListTests.elm` truncated to N children: N=60 K=60 P=4
ran in 4.7 s; N=65 K=65 same shape hung). `combine` builds a wide tree
of pending Cache+dispatch chains; the runtime work to resolve that tree
is super-linear past ~60 entries on this hardware.

Batching the calls in groups under the cap keeps the runtime in the
known-good envelope at the cost of a little wall-clock parallelism
between batches. For ListTests-shape suites at pool=4 with one
dispatch per top-level describe child, `batchSize ≈ 30` finishes the
77-child workload in two batches.

When the per-test split path is disabled (`perChildSplitThreshold`
default 1000), each test file produces one chunk, so `batchedCombine`
behaves like a plain `BackendTask.combine` regardless of `batchSize`.

-}
batchedCombine :
    Int
    -> List (BackendTask FatalError a)
    -> BackendTask FatalError (List a)
batchedCombine batchSize tasks =
    if batchSize <= 0 || List.length tasks <= batchSize then
        BackendTask.combine tasks

    else
        tasks
            |> chunkList batchSize
            |> List.foldl
                (\batch acc ->
                    acc
                        |> BackendTask.andThen
                            (\done ->
                                BackendTask.combine batch
                                    |> BackendTask.map (\res -> done ++ res)
                            )
                )
                (BackendTask.succeed [])





-- ── Worker wire format helpers ──
--
-- Per-task input/output envelopes for `runOn`. Shared payload (sent
-- once per worker at startup) lives in `TestRunnerCommon` so both
-- sides reference the same Wire3-derived codec.


encodeWorkerTaskInput : { imports : List String, expression : String, sourceOverrides : List String } -> Bytes
encodeWorkerTaskInput input =
    encodeJsonBytes
        (Json.Encode.object
            [ ( "imports", Json.Encode.list Json.Encode.string input.imports )
            , ( "expression", Json.Encode.string input.expression )
            , ( "sourceOverrides", Json.Encode.list Json.Encode.string input.sourceOverrides )
            ]
        )


workerTaskOutputDecoder : BD.Decoder String
workerTaskOutputDecoder =
    decodeJsonBytes (Json.Decode.field "output" Json.Decode.string)


encodeJsonBytes : Json.Encode.Value -> Bytes
encodeJsonBytes value =
    let
        utf8 : Bytes
        utf8 =
            BE.encode (BE.string (Json.Encode.encode 0 value))
    in
    BE.encode
        (BE.sequence
            [ BE.unsignedInt32 Bytes.BE (Bytes.width utf8)
            , BE.bytes utf8
            ]
        )


decodeJsonBytes : Json.Decode.Decoder a -> BD.Decoder a
decodeJsonBytes jsonDecoder =
    BD.unsignedInt32 Bytes.BE
        |> BD.andThen BD.string
        |> BD.andThen
            (\jsonStr ->
                case Json.Decode.decodeString jsonDecoder jsonStr of
                    Ok value ->
                        BD.succeed value

                    Err _ ->
                        BD.fail
            )


{-| Discover test files: use --test if provided, otherwise find all tests/**/*.elm that import Test.
-}
resolveTestModuleNames : List String -> BackendTask FatalError (List String)
resolveTestModuleNames testFiles =
    testFiles
        |> List.map
            (\filePath ->
                File.rawFile filePath
                    |> BackendTask.allowFatal
                    |> BackendTask.map
                        (\content ->
                            DepGraph.parseModuleName content
                        )
            )
        |> BackendTask.sequence
        |> BackendTask.map (List.filterMap identity)


resolveTestFiles : Config -> BackendTask FatalError (List String)
resolveTestFiles config =
    case config.testFile of
        Just explicit ->
            BackendTask.succeed (String.split "," explicit)

        Nothing ->
            Glob.fromStringWithOptions
                (let
                    o =
                        Glob.defaultOptions
                 in
                 { o | include = Glob.OnlyFiles }
                )
                "tests/**/*.elm"
                |> BackendTask.andThen
                    (\files ->
                        files
                            |> List.map
                                (\filePath ->
                                    File.rawFile filePath
                                        |> BackendTask.allowFatal
                                        |> BackendTask.map (\content -> ( filePath, content ))
                                )
                            |> BackendTask.sequence
                            |> BackendTask.map
                                (\pairs ->
                                    pairs
                                        |> List.filter
                                            (\( _, content ) ->
                                                List.member "Test" (DepGraph.parseImports content)
                                            )
                                        |> List.map Tuple.first
                                        |> List.sort
                                )
                    )



ensureDeps : BackendTask FatalError ()
ensureDeps =
    File.rawFile "elm.json"
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\raw ->
                let
                    isPackage =
                        String.contains "\"type\": \"package\"" raw
                            || String.contains "\"type\":\"package\"" raw

                    fetchCmd =
                        if isPackage then
                            Script.exec "elm" [ "make", "--docs", "/tmp/elm-test-runner-docs.json" ]

                        else
                            Glob.fromStringWithOptions
                                (let
                                    o =
                                        Glob.defaultOptions
                                 in
                                 { o | include = Glob.OnlyFiles }
                                )
                                "src/**/*.elm"
                                |> BackendTask.andThen
                                    (\files ->
                                        case files of
                                            first :: _ ->
                                                Script.exec "elm" [ "make", first, "--output", "/dev/null" ]

                                            [] ->
                                                BackendTask.succeed ()
                                    )
                in
                fetchCmd
                    |> BackendTask.toResult
                    |> BackendTask.map (\_ -> ())
            )


parseReportFormat : String -> ReportFormat
parseReportFormat rawValue =
    case String.toLower (String.trim rawValue) of
        "json" ->
            ReportJson

        _ ->
            ReportConsole


simpleTestRunnerSource : Config -> String
simpleTestRunnerSource config =
    String.join "\n"
        [ "module SimpleTestRunner exposing (runToString)"
        , "import Expect"
        , "import Random"
        , "import Test exposing (Test)"
        , "import Test.Runner"
        , "runToString suite ="
        , "    let"
        , "        runners = case Test.Runner.fromTest "
            ++ String.fromInt config.fuzzRuns
            ++ " (Random.initialSeed "
            ++ String.fromInt config.seed
            ++ ") suite of"
        , "            Test.Runner.Plain list -> Ok list"
        , "            Test.Runner.Only list -> Ok list"
        , "            Test.Runner.Skipping list -> Ok list"
        , "            Test.Runner.Invalid msg -> Err msg"
        , "    in"
        , "    case runners of"
        , "        Err msg -> \"0,1,1\\nFAIL:Invalid test suite: \" ++ msg"
        , "        Ok runnerList ->"
        , "            let"
        , "                results = List.map runOneRunner runnerList"
        , "                passCount = List.length (List.filter .passed results)"
        , "                failCount = List.length results - passCount"
        , "                formatResult r = if r.passed then \"PASS:\" ++ r.label else \"FAIL:\" ++ r.label ++ \" | \" ++ r.message"
        , "            in"
        , "            String.fromInt passCount ++ \",\" ++ String.fromInt failCount ++ \",\" ++ String.fromInt (List.length results) ++ \"\\n\" ++ (List.map formatResult results |> String.join \"\\n\")"
        , "runOneRunner runner ="
        , "    let"
        , "        labelPath = List.reverse runner.labels |> String.join \" > \""
        , "        expectations = runner.run ()"
        , "        failures = expectations |> List.filterMap (\\expectation -> Test.Runner.getFailureReason expectation |> Maybe.map .description)"
        , "        passed = List.isEmpty failures"
        , "    in"
        , "    { passed = passed, label = labelPath, message = if passed then \"\" else String.join \"; \" failures }"
        ]


kernelPackages : Set.Set String
kernelPackages =
    Set.fromList
        [ "elm/html"
        , "elm/virtual-dom"
        , "elm/browser"
        , "elm/http"
        , "elm/file"

        -- elm/url is NOT skipped: its source files load and parse fine,
        -- and `patchSource` below rewrites `Url.percentEncode` /
        -- `percentDecode` bodies (which would otherwise bottom out at
        -- `Elm.Kernel.Url.*`) to pure-Elm implementations.
        ]


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
            "internal TCO signal"


patchSource : String -> String
patchSource source =
    source
        |> patchTestRunner
        |> patchUrl


patchTestRunner : String -> String
patchTestRunner source =
    if String.contains "runThunk =\n    Elm.Kernel.Test.runThunk" source then
        source
            |> String.replace
                "runThunk =\n    Elm.Kernel.Test.runThunk"
                "runThunk fn =\n    Ok (fn ())"

    else
        source


{-| Rewrite `elm/url`'s `Url.elm` so `percentEncode` / `percentDecode`
don't reach `Elm.Kernel.Url.*` (which our interpreter has no native
implementation for). The replacement is a self-contained pure-Elm
version inlined into the `Url` module itself, using standard
`Char.toCode` / `String.toList` / UTF-8 bit-twiddling. Only triggers on
the specific body of `elm/url`'s stock `Url.elm`, so if the source ever
changes we'll notice (the patch silently no-ops and tests that need
percent-encoding will fail).
-}
patchUrl : String -> String
patchUrl source =
    if String.contains "Elm.Kernel.Url.percentEncode" source then
        source
            |> String.replace "import Elm.Kernel.Url\n" ""
            |> String.replace
                "percentEncode : String -> String\npercentEncode =\n  Elm.Kernel.Url.percentEncode"
                percentEncodePatched
            |> String.replace
                "percentDecode : String -> Maybe String\npercentDecode =\n  Elm.Kernel.Url.percentDecode"
                percentDecodePatched

    else
        source


percentEncodePatched : String
percentEncodePatched =
    String.join "\n"
        [ "percentEncode : String -> String"
        , "percentEncode str ="
        , "    let"
        , "        hexChar n ="
        , "            if n < 10 then Char.fromCode (0x30 + n)"
        , "            else Char.fromCode (0x41 + n - 10)"
        , "        hexByte n ="
        , "            String.fromList [ '%', hexChar (n // 16), hexChar (modBy 16 n) ]"
        , "        encodeByte code ="
        , "            if (code >= 0x41 && code <= 0x5A)"
        , "                || (code >= 0x61 && code <= 0x7A)"
        , "                || (code >= 0x30 && code <= 0x39)"
        , "                || code == 0x2D || code == 0x5F || code == 0x2E || code == 0x7E"
        , "            then"
        , "                String.fromChar (Char.fromCode code)"
        , "            else"
        , "                hexByte code"
        , "        encodeCodePoint code ="
        , "            if code < 0x80 then"
        , "                encodeByte code"
        , "            else if code < 0x800 then"
        , "                hexByte (0xC0 + (code // 64)) ++ hexByte (0x80 + modBy 64 code)"
        , "            else if code < 0x10000 then"
        , "                hexByte (0xE0 + (code // 4096))"
        , "                    ++ hexByte (0x80 + modBy 64 (code // 64))"
        , "                    ++ hexByte (0x80 + modBy 64 code)"
        , "            else"
        , "                hexByte (0xF0 + (code // 262144))"
        , "                    ++ hexByte (0x80 + modBy 64 (code // 4096))"
        , "                    ++ hexByte (0x80 + modBy 64 (code // 64))"
        , "                    ++ hexByte (0x80 + modBy 64 code)"
        , "    in"
        , "    String.foldr (\\c acc -> encodeCodePoint (Char.toCode c) ++ acc) \"\" str"
        ]


percentDecodePatched : String
percentDecodePatched =
    String.join "\n"
        [ "percentDecode : String -> Maybe String"
        , "percentDecode str ="
        , "    let"
        , "        hexVal c ="
        , "            let code = Char.toCode c"
        , "            in"
        , "            if code >= 0x30 && code <= 0x39 then Just (code - 0x30)"
        , "            else if code >= 0x41 && code <= 0x46 then Just (code - 0x41 + 10)"
        , "            else if code >= 0x61 && code <= 0x66 then Just (code - 0x61 + 10)"
        , "            else Nothing"
        , "        helper remaining acc ="
        , "            case remaining of"
        , "                [] ->"
        , "                    Just (String.fromList (List.reverse acc))"
        , "                '%' :: hi :: lo :: rest ->"
        , "                    case ( hexVal hi, hexVal lo ) of"
        , "                        ( Just h, Just l ) -> helper rest (Char.fromCode (h * 16 + l) :: acc)"
        , "                        _ -> Nothing"
        , "                c :: rest ->"
        , "                    helper rest (c :: acc)"
        , "    in"
        , "    helper (String.toList str) []"
        ]
