module PureTestRunner exposing (run)

{-| A test runner that uses ElmProject.evalWith to run tests out-of-process.

Instead of importing and executing tests directly, this generates a wrapper
module that calls `TestRunnerHelper.runToJson SampleTests.suite`, compiles it
with `elm make`, and runs it with `node`. The JSON results are cached via the
eval infrastructure, so unchanged tests are instant on subsequent runs.

-}

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Random
import Cache
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import ElmProject
import FatalError exposing (FatalError)
import Json.Decode
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import TestAnalysis


run : Script
run =
    Script.withCliOptions programConfig task


type alias Config =
    { buildDirectory : Path
    }


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.requiredKeywordArg "build"
                        |> Option.map Path.path
                        |> Option.withDescription "Build folder for the test cache"
                    )
            )


task : Config -> BackendTask FatalError ()
task config =
    BackendTask.Extra.profiling "pure-test-runner" <|
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Hashing source files and building dependency graph") <| \_ ->
        Do.do (ElmProject.fromPath (Path.path ".")) <| \project ->
        Do.exec "mkdir" [ "-p", Path.toString config.buildDirectory ] <| \_ ->
        Do.allowFatal (File.rawFile "src/SampleTests.elm") <| \testSource ->
        let
            moduleUsesFuzz =
                TestAnalysis.usesFuzz testSource
        in
        Do.do
            (if moduleUsesFuzz then
                BackendTask.Random.int32
                    |> BackendTask.map String.fromInt

             else
                BackendTask.succeed ""
            )
        <| \seed ->
        let
            expression =
                if moduleUsesFuzz then
                    "TestRunnerHelper.runToJsonWithSeed " ++ seed ++ " SampleTests.suite"

                else
                    "TestRunnerHelper.runToJson SampleTests.suite"

            ranMessage =
                if moduleUsesFuzz then
                    "Ran tests (seed: " ++ seed ++ ")"

                else
                    "Ran tests"
        in
        Do.do
            (Cache.run { jobs = Nothing } config.buildDirectory
                (ElmProject.evalWith project
                    { imports = [ "TestRunnerHelper", "SampleTests" ]
                    , expression = expression
                    }
                    Cache.succeed
                    |> Cache.timed "Running tests" ranMessage
                )
            )
        <| \result ->
        Do.allowFatal (File.rawFile (Path.toString result.output)) <| \jsonOutput ->
        displayResults jsonOutput


type alias TestResult =
    { passed : Bool
    , labels : List String
    , message : String
    }


testResultDecoder : Json.Decode.Decoder TestResult
testResultDecoder =
    Json.Decode.map3 TestResult
        (Json.Decode.field "passed" Json.Decode.bool)
        (Json.Decode.field "labels" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "message" Json.Decode.string)


type alias TestReport =
    { passed : Int
    , failed : Int
    , total : Int
    , results : List TestResult
    }


testReportDecoder : Json.Decode.Decoder TestReport
testReportDecoder =
    Json.Decode.map4 TestReport
        (Json.Decode.field "passed" Json.Decode.int)
        (Json.Decode.field "failed" Json.Decode.int)
        (Json.Decode.field "total" Json.Decode.int)
        (Json.Decode.field "results" (Json.Decode.list testResultDecoder))


displayResults : String -> BackendTask FatalError ()
displayResults jsonOutput =
    case Json.Decode.decodeString testReportDecoder jsonOutput of
        Err err ->
            BackendTask.fail
                (FatalError.fromString
                    ("Failed to decode test results: " ++ Json.Decode.errorToString err ++ "\nRaw output: " ++ jsonOutput)
                )

        Ok report ->
            let
                passResults : List TestResult
                passResults =
                    List.filter .passed report.results

                failResults : List TestResult
                failResults =
                    List.filter (not << .passed) report.results
            in
            Do.each passResults
                (\result ->
                    Script.log
                        (Ansi.Color.fontColor Ansi.Color.green
                            ("  ✓ " ++ String.join " > " result.labels)
                        )
                )
            <| \_ ->
            Do.each failResults
                (\result ->
                    Script.log
                        (Ansi.Color.fontColor Ansi.Color.red
                            ("  ✗ "
                                ++ String.join " > " result.labels
                                ++ "\n    "
                                ++ result.message
                            )
                        )
                )
            <| \_ ->
            Do.log
                (let
                    summary =
                        "{ \"passed\": "
                            ++ String.fromInt report.passed
                            ++ ", \"failed\": "
                            ++ String.fromInt report.failed
                            ++ ", \"total\": "
                            ++ String.fromInt report.total
                            ++ " }"

                    color =
                        if report.failed == 0 then
                            Ansi.Color.fontColor Ansi.Color.brightGreen

                        else
                            Ansi.Color.fontColor Ansi.Color.brightRed
                 in
                 color ("\n" ++ summary)
                )
            <| \_ ->
            Do.noop
