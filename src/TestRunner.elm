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
import FatalError exposing (FatalError)
import InterpreterProject exposing (InterpreterProject)
import Json.Decode
import Json.Encode
import NormalizationFlags
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
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
            Do.do
                (BackendTask.Parallel.worker
                    { workerModule = "TestFileWorker"
                    , shared =
                        TestRunnerCommon.encodeWorkerShared
                            { projectDir = "."
                            , sourceDirectories = allDirectories
                            , testModuleNames = testModuleNames
                            }
                    }
                )
            <| \worker ->
            Do.do
                (testFiles
                    |> List.map (\testFile -> runTestFileViaWorker config loaded.project worker testFile)
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
         , "  build_base_user_env_ms=" ++ String.fromInt profile.buildBaseUserEnvMs
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
runTestFileViaWorker : Config -> InterpreterProject -> BackendTask.Parallel.Worker -> String -> BackendTask FatalError TestFileResult
runTestFileViaWorker config project worker testFile =
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
                planChildExpressions testModuleName testSource testValues

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
                |> BackendTask.combine
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
planChildExpressions : String -> String -> List String -> List String
planChildExpressions testModuleName testSource testValues =
    case testValues of
        [ single ] ->
            case TestAnalysis.extractDescribeChildren single testSource of
                Just children ->
                    if List.length children >= perChildSplitThreshold then
                        children

                    else
                        [ testModuleName ++ "." ++ single ]

                Nothing ->
                    [ testModuleName ++ "." ++ single ]

        multiple ->
            [ "Test.describe \"" ++ testModuleName ++ "\" [" ++ String.join ", " (List.map (\v -> testModuleName ++ "." ++ v) multiple) ++ "]" ]


{-| Split a file's top-level `describe` into per-child dispatches when
its child count meets this threshold. Below this, the per-task overhead
(dispatch + cache check + IPC) dominates the parallel-mass win.

**Currently disabled (1000)** while the per-child runtime cost is being
diagnosed. Bench data (core-extra ListTests, 77 children):

    take=3   →  1.7 s eval (works)
    take=20  →  2.1 s eval (works)
    take=40  →  4.6 s eval (works)
    take=60  →  hangs at >>5 min CPU
    take=77  →  hangs at >>5 min CPU

Linear extrapolation says 60 should be ~7 s. Something nonlinear blows
up between 40 and 60 children, regardless of `BackendTask.sequence` vs
`combine` and regardless of `Cache.runWith` wrapping. The `dispatchOne`
function is exercised correctly (`Debug.log` confirms 77 children
parsed and the first few dispatches start), so the issue is in the
per-child eval cost compounding — possibly evaluator state growing
across many small evals against the same `baseUserEnv`, possibly Node
Worker queue pressure, possibly something in the cache-key computation
when many sibling cache entries land in the same directory.

See `.scratch/parallel-ceiling.md` 2026-04-21 entry for the full
investigation. The per-child split machinery (`planChildExpressions` +
`dispatchOne` over a `Cache.HashSet`) is left in place as a
known-good-to-N=40 hook for whatever fix lands.

-}
perChildSplitThreshold : Int
perChildSplitThreshold =
    1000



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
