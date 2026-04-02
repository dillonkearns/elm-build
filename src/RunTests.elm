module RunTests exposing (run)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Random
import CoverageTests
import DepGraphTests
import MutationReportTests
import MutationTestRunnerTest
import MutatorTests
import Test
import Test.Runner
import TestAnalysisTests


run : Script
run =
    Script.withoutCliOptions runAll


runAll : BackendTask FatalError ()
runAll =
    let
        seed =
            Random.initialSeed 42

        allSuites =
            Test.describe "All" [ TestAnalysisTests.suite, MutatorTests.suite, MutationTestRunnerTest.suite, DepGraphTests.suite, MutationReportTests.suite, CoverageTests.suite ]

        runners =
            case Test.Runner.fromTest 100 seed allSuites of
                Test.Runner.Plain list ->
                    list

                Test.Runner.Only list ->
                    list

                Test.Runner.Skipping list ->
                    list

                Test.Runner.Invalid _ ->
                    []

        results =
            runners
                |> List.map
                    (\runner ->
                        let
                            label =
                                String.join " > " (List.reverse runner.labels)

                            failures =
                                runner.run ()
                                    |> List.filterMap
                                        (Test.Runner.getFailureReason >> Maybe.map .description)
                        in
                        { label = label, passed = List.isEmpty failures, message = String.join "\n" failures }
                    )

        passed =
            List.filter .passed results

        failed =
            List.filter (not << .passed) results

        passOutput =
            passed |> List.map (\r -> Ansi.Color.fontColor Ansi.Color.green ("  ✓ " ++ r.label)) |> String.join "\n"

        failOutput =
            failed |> List.map (\r -> Ansi.Color.fontColor Ansi.Color.red ("  ✗ " ++ r.label ++ "\n    " ++ r.message)) |> String.join "\n\n"

        summary =
            String.fromInt (List.length passed) ++ " passed, " ++ String.fromInt (List.length failed) ++ " failed"
    in
    Script.log (passOutput ++ "\n" ++ failOutput)
        |> BackendTask.andThen
            (\() ->
                if List.isEmpty failed then
                    Script.log (Ansi.Color.fontColor Ansi.Color.brightGreen ("\n" ++ summary))

                else
                    Script.log (Ansi.Color.fontColor Ansi.Color.brightRed ("\n" ++ summary))
                        |> BackendTask.andThen (\() -> BackendTask.fail (FatalError.build { title = "Tests Failed", body = "" }))
            )
