module BenchSingleFile exposing (run)

{-| Diagnostic harness for the Phase 2 ListTests-in-worker slowdown.

Loads the project once on the main thread, then runs the same eval
TestFileWorker would dispatch (`SimpleTestRunner.runToString
(ListTests.suite)` against the main-thread `InterpreterProject`), three
times, timing each call. First run includes any cold cache warmup;
runs 2 and 3 are the steady-state main-thread eval cost we want to
compare against the worker's ~11 s for the same eval.

Usage (run from a project directory that has tests/ListTests.elm):

    npx elm-pages run /path/to/elm-build/src/BenchSingleFile.elm \
        --test tests/ListTests.elm

Reports:

    Load: NNNms
    evalSimple #1: NNNms
    evalSimple #2: NNNms
    evalSimple #3: NNNms

If runs 2/3 are ~11 s, the slowdown is in `evalSimple` regardless of
worker context (look at the eval path itself). If they're ~3 s, the
worker context is the culprit (look at `parallel-worker.js`'s Elm
runtime setup, JIT warmup, or evalSimple-vs-evalWithFileOverrides path
divergence).

-}

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Time
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import DepGraph
import FatalError exposing (FatalError)
import InterpreterProject exposing (InterpreterProject)
import Pages.Script as Script exposing (Script)
import Path
import Set
import TestAnalysis
import TestRunner
import Time


type alias BenchConfig =
    { testFile : String
    }


programConfig : Program.Config BenchConfig
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build BenchConfig
                |> OptionsParser.with
                    (Option.requiredKeywordArg "test"
                        |> Option.withDescription "Single test file path (e.g. tests/ListTests.elm)"
                    )
            )


run : Script
run =
    Script.withCliOptions programConfig task


task : BenchConfig -> BackendTask FatalError ()
task config =
    Do.do BackendTask.Time.now <| \loadStart ->
    Do.do
        (InterpreterProject.loadWith
            { projectDir = Path.path "."
            , skipPackages = TestRunner.kernelPackages
            , patchSource = TestRunner.patchSource
            , patchUserSource = \_ source -> source
            , extraSourceFiles = []
            , extraReachableImports = [ "Test", "Fuzz", "Expect", "Test.Runner" ]
            , sourceDirectories = Just [ "tests", "src" ]
            , normalizationRoots = Just []
            }
        )
    <| \project ->
    Do.do BackendTask.Time.now <| \loadEnd ->
    Do.do (Script.log ("Load: " ++ String.fromInt (Time.posixToMillis loadEnd - Time.posixToMillis loadStart) ++ "ms")) <| \_ ->
    Do.allowFatal (File.rawFile config.testFile) <| \testSource ->
    let
        testModuleName : String
        testModuleName =
            DepGraph.parseModuleName testSource |> Maybe.withDefault "Tests"

        testValues : List String
        testValues =
            TestAnalysis.discoverTestValues testSource

        suiteExpr : String
        suiteExpr =
            case testValues of
                [ single ] ->
                    testModuleName ++ "." ++ single

                multiple ->
                    "Test.describe \"" ++ testModuleName ++ "\" [" ++ String.join ", " (List.map (\v -> testModuleName ++ "." ++ v) multiple) ++ "]"

        evalSpec : { imports : List String, expression : String, sourceOverrides : List String }
        evalSpec =
            { imports = [ "SimpleTestRunner", "Test", testModuleName ]
            , expression = "SimpleTestRunner.runToString (" ++ suiteExpr ++ ")"
            , sourceOverrides = [ simpleTestRunnerSource 1 42 ]
            }
    in
    timeEval "#1" project evalSpec <| \_ ->
    timeEval "#2" project evalSpec <| \_ ->
    timeEval "#3" project evalSpec <| \_ ->
    BackendTask.succeed ()


timeEval : String -> InterpreterProject -> { imports : List String, expression : String, sourceOverrides : List String } -> (() -> BackendTask FatalError ()) -> BackendTask FatalError ()
timeEval label project evalSpec k =
    Do.do BackendTask.Time.now <| \start ->
    Do.do (InterpreterProject.evalSimple project evalSpec) <| \_ ->
    Do.do BackendTask.Time.now <| \end ->
    Do.do (Script.log ("evalSimple " ++ label ++ ": " ++ String.fromInt (Time.posixToMillis end - Time.posixToMillis start) ++ "ms")) <| \_ ->
    k ()


{-| Inlined copy of `TestRunner.simpleTestRunnerSource`, parameterised
on `fuzzRuns` and `seed` directly instead of the full `Config` record.
Kept in-sync with TestRunner.elm:986.
-}
simpleTestRunnerSource : Int -> Int -> String
simpleTestRunnerSource fuzzRuns seed =
    String.join "\n"
        [ "module SimpleTestRunner exposing (runToString)"
        , "import Expect"
        , "import Random"
        , "import Test exposing (Test)"
        , "import Test.Runner"
        , "runToString suite ="
        , "    let"
        , "        runners = case Test.Runner.fromTest "
            ++ String.fromInt fuzzRuns
            ++ " (Random.initialSeed "
            ++ String.fromInt seed
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
