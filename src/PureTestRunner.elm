module PureTestRunner exposing (run)

{-| A proof-of-concept test runner built entirely in pure Elm.

Instead of shelling out to elm-test, this uses Test.Runner to enumerate
and execute test cases directly. Each test result is cached via the
Cache.compute primitive — if the source files haven't changed, individual
test results are returned from cache without re-execution.

This demonstrates the key insight: Elm tests are pure functions, so their
results can be content-addressed and cached just like any other build artifact.

-}

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Glob as Glob
import Cache exposing (FileOrDirectory)
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Expect
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import Random
import SampleTests
import Test
import Test.Runner
import Test.Runner.Failure


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
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Hashing source files") <| \_ ->
        Do.do (hashSourceFiles (Path.path ".")) <| \sourceInputs ->
        Do.exec "mkdir" [ "-p", Path.toString config.buildDirectory ] <| \_ ->
        Do.do
            (Cache.run config.buildDirectory
                (runAllTests sourceInputs
                    |> Cache.timed "Running tests" "Ran tests"
                )
            )
        <| \result ->
        Do.allowFatal (File.rawFile (Path.toString result.output)) <| \jsonOutput ->
        displayPureResults jsonOutput


{-| Hash the source files that tests depend on.
Includes all .elm files in src/ plus elm.json (so dependency changes invalidate the cache).
-}
hashSourceFiles : Path -> BackendTask FatalError (List ( Path, Cache.Monad FileOrDirectory ))
hashSourceFiles projectDir =
    let
        p : String
        p =
            Path.toString projectDir
    in
    Glob.fromStringWithOptions
        (let
            o : Glob.Options
            o =
                Glob.defaultOptions
         in
         { o | include = Glob.OnlyFiles }
        )
        (p ++ "/src/**/*.elm")
        |> BackendTask.andThen
            (\files ->
                files
                    |> List.filter (\f -> not (String.contains "elm-stuff" f))
                    |> (::) (p ++ "/elm.json")
                    |> List.sort
                    |> List.map Path.path
                    |> Cache.inputs
            )


{-| Run all tests from SampleTests.suite, caching each individually.
-}
runAllTests : List ( Path, Cache.Monad FileOrDirectory ) -> Cache.Monad FileOrDirectory
runAllTests sourceInputs =
    let
        -- Hash all source files together as the dependency baseline
        combinedDepsMonad : Cache.Monad FileOrDirectory
        combinedDepsMonad =
            sourceInputs
                |> List.map
                    (\( absPath, monad ) ->
                        Cache.do monad <| \hash ->
                        Cache.succeed { filename = absPath, hash = hash }
                    )
                |> Cache.sequence
                |> Cache.andThen Cache.combine

        -- Use a deterministic seed derived from "pure-test-runner"
        seed : Random.Seed
        seed =
            Random.initialSeed 42

        -- Get all runners from the test suite
        runners : List Test.Runner.Runner
        runners =
            case Test.Runner.fromTest 100 seed SampleTests.suite of
                Test.Runner.Plain list ->
                    list

                Test.Runner.Only list ->
                    list

                Test.Runner.Skipping list ->
                    list

                Test.Runner.Invalid msg ->
                    []
    in
    Cache.do combinedDepsMonad <| \depsHash ->
    Cache.do
        (runners
            |> List.map (runSingleTest depsHash)
            |> Cache.sequence
        )
    <| \testResults ->
    -- Combine all test results into a single report
    let
        allResults : List String
        allResults =
            List.map Tuple.second testResults

        passCount : Int
        passCount =
            List.length (List.filter Tuple.first testResults)

        failCount : Int
        failCount =
            List.length testResults - passCount

        report : String
        report =
            String.join "\n"
                ([ "{ \"passed\": " ++ String.fromInt passCount
                    ++ ", \"failed\": " ++ String.fromInt failCount
                    ++ ", \"total\": " ++ String.fromInt (List.length testResults)
                    ++ " }"
                 ]
                    ++ allResults
                )
    in
    Cache.writeFile report Cache.succeed


{-| Run a single test case, caching the result via Cache.compute.

The cache key is derived from:
  - The test's label path (its unique identity)
  - The hash of all source dependencies

If the source files haven't changed, the test result is returned from
cache without executing the test at all.

-}
runSingleTest : FileOrDirectory -> Test.Runner.Runner -> Cache.Monad ( Bool, String )
runSingleTest depsHash runner =
    let
        labelPath : List String
        labelPath =
            List.reverse runner.labels
    in
    Cache.compute labelPath depsHash
        (\() ->
            -- This thunk only executes on cache miss
            let
                expectations : List Expect.Expectation
                expectations =
                    runner.run ()

                failures : List { labels : List String, given : Maybe String, description : String }
                failures =
                    expectations
                        |> List.filterMap
                            (\expectation ->
                                Test.Runner.getFailureReason expectation
                                    |> Maybe.map
                                        (\failure ->
                                            { labels = labelPath
                                            , given = failure.given
                                            , description = failure.description
                                            }
                                        )
                            )

                passed : Bool
                passed =
                    List.isEmpty failures

                status : String
                status =
                    if passed then
                        "pass"

                    else
                        "FAIL"
            in
            status
                ++ " | "
                ++ String.join " > " labelPath
                ++ (if passed then
                        ""

                    else
                        failures
                            |> List.map (\f -> "\n    " ++ f.description)
                            |> String.join ""
                   )
        )
    <| \resultHash ->
    Cache.withFile resultHash
        (\content ->
            Cache.succeed
                ( String.startsWith "pass" content
                , content
                )
        )
        Cache.succeed


displayPureResults : String -> BackendTask FatalError ()
displayPureResults report =
    let
        lines : List String
        lines =
            String.lines report

        summaryLine : String
        summaryLine =
            List.head lines |> Maybe.withDefault ""

        testLines : List String
        testLines =
            List.drop 1 lines
                |> List.filter (not << String.isEmpty)

        passLines : List String
        passLines =
            List.filter (String.startsWith "pass") testLines

        failLines : List String
        failLines =
            List.filter (String.startsWith "FAIL") testLines
    in
    Do.each passLines
        (\line ->
            Script.log (Ansi.Color.fontColor Ansi.Color.green ("  ✓ " ++ String.dropLeft 7 line))
        )
    <| \_ ->
    Do.each failLines
        (\line ->
            Script.log (Ansi.Color.fontColor Ansi.Color.red ("  ✗ " ++ String.dropLeft 7 line))
        )
    <| \_ ->
    Do.log
        (let
            color =
                if List.isEmpty failLines then
                    Ansi.Color.fontColor Ansi.Color.brightGreen

                else
                    Ansi.Color.fontColor Ansi.Color.brightRed
         in
         color ("\n" ++ summaryLine)
        )
    <| \_ ->
    Do.noop
