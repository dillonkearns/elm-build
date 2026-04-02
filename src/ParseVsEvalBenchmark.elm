module ParseVsEvalBenchmark exposing (run)

{-| Benchmark: measure parse time vs eval time per mutation.

Runs the same mutation both ways:
1. String-based: evalWithEnv parses the source string each time
2. AST-based: parseProjectSources once, evalWithEnvFromFiles per mutation

-}

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Time
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Elm.Syntax.Expression exposing (Expression(..))
import Eval.Module
import FatalError exposing (FatalError)
import Mutator
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import ProjectSources
import Set
import Time
import Types


run : Script
run =
    Script.withCliOptions programConfig task


type alias Config =
    { mutateFile : String
    , testFile : String
    }


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.optionalKeywordArg "mutate"
                        |> Option.map (Maybe.withDefault "src/Validator.elm")
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "test"
                        |> Option.map (Maybe.withDefault "src/ValidatorTests.elm")
                    )
            )


task : Config -> BackendTask FatalError ()
task config =
    Do.do
        (ProjectSources.loadProjectSources
            { projectDir = Path.path "."
            , userSourceDirectories = [ "src", "tests" ]
            , targetFile = config.testFile
            , skipPackages = kernelPackages
            }
            |> BackendTask.map (List.map patchSource)
        )
    <| \allSources ->
    Do.allowFatal (File.rawFile config.mutateFile) <| \mutateSource ->
    let
        mutations =
            Mutator.generateMutations mutateSource

        mutationCount =
            List.length mutations

        -- Build the env once for both approaches
        envResult =
            Eval.Module.buildProjectEnv (allSources ++ [ wrapperSource config.testFile, simpleTestRunnerSource ])
    in
    case envResult of
        Err _ ->
            BackendTask.fail (FatalError.fromString "Failed to build env")

        Ok projectEnv ->
            -- Pre-parse user sources for the AST approach
            let
                userAndWrapper =
                    allSources ++ [ simpleTestRunnerSource, wrapperSource config.testFile ]

                preParsedResult =
                    Eval.Module.parseProjectSources userAndWrapper
            in
            case preParsedResult of
                Err _ ->
                    BackendTask.fail (FatalError.fromString "Failed to pre-parse sources")

                Ok preParsed ->
                    -- Benchmark 1: String-based (current approach)
                    Do.do BackendTask.Time.now <| \t1 ->
                    let
                        _ =
                            mutations
                                |> List.map
                                    (\mutation ->
                                        Eval.Module.evalWithEnv projectEnv
                                            [ mutation.mutatedSource, simpleTestRunnerSource, wrapperSource config.testFile ]
                                            (FunctionOrValue [] "wrapperResult__")
                                    )
                    in
                    Do.do BackendTask.Time.now <| \t2 ->
                    -- Benchmark 2: Zero-parse (pre-parse EVERYTHING, no parse per mutation)
                    -- This is the theoretical max for AST-based: all modules pre-parsed
                    let
                        allPreParsed =
                            mutations
                                |> List.map
                                    (\mutation ->
                                        Eval.Module.parseProjectSources
                                            [ mutation.mutatedSource, simpleTestRunnerSource, wrapperSource config.testFile ]
                                            |> Result.withDefault []
                                    )

                        _ =
                            allPreParsed
                                |> List.map
                                    (\parsed ->
                                        Eval.Module.evalWithEnvFromFiles projectEnv (List.map .file parsed) (FunctionOrValue [] "wrapperResult__")
                                    )
                    in
                    Do.do BackendTask.Time.now <| \t3 ->
                    let
                        stringMs =
                            Time.posixToMillis t2 - Time.posixToMillis t1

                        astMs =
                            Time.posixToMillis t3 - Time.posixToMillis t2
                    in
                    Script.log
                        ("Mutations: "
                            ++ String.fromInt mutationCount
                            ++ "\nString-based: "
                            ++ String.fromInt stringMs
                            ++ "ms ("
                            ++ String.fromInt (stringMs // mutationCount)
                            ++ "ms/mutation)"
                            ++ "\nAST-based:    "
                            ++ String.fromInt astMs
                            ++ "ms ("
                            ++ String.fromInt (astMs // mutationCount)
                            ++ "ms/mutation)"
                            ++ "\nSpeedup:      "
                            ++ (if astMs > 0 then
                                    String.fromFloat (toFloat stringMs / toFloat astMs)
                                        |> String.left 4

                                else
                                    "∞"
                               )
                            ++ "x"
                        )


wrapperSource : String -> String
wrapperSource testFile =
    let
        moduleName =
            testFile
                |> String.replace "src/" ""
                |> String.replace "tests/" ""
                |> String.replace ".elm" ""
                |> String.replace "/" "."
    in
    "module MutationTestWrapper__ exposing (wrapperResult__)\nimport SimpleTestRunner\nimport Test\nimport "
        ++ moduleName
        ++ "\nwrapperResult__ = SimpleTestRunner.runToString "
        ++ moduleName
        ++ ".suite\n"


simpleTestRunnerSource : String
simpleTestRunnerSource =
    String.join "\n"
        [ "module SimpleTestRunner exposing (runToString)"
        , "import Expect"
        , "import Random"
        , "import Test exposing (Test)"
        , "import Test.Runner"
        , "runToString suite ="
        , "    let"
        , "        runners = case Test.Runner.fromTest 1 (Random.initialSeed 42) suite of"
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
    Set.fromList [ "elm/json", "elm/regex", "elm/html", "elm/virtual-dom", "elm/bytes", "elm/browser", "elm/http", "elm/file", "elm/url" ]


patchSource : String -> String
patchSource source =
    if String.contains "runThunk =\n    Elm.Kernel.Test.runThunk" source then
        source |> String.replace "runThunk =\n    Elm.Kernel.Test.runThunk" "runThunk fn =\n    Ok (fn ())"

    else
        source
