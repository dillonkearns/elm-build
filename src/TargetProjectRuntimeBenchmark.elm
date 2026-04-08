module TargetProjectRuntimeBenchmark exposing (run)

import BackendTask exposing (BackendTask)
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import ReviewRunner


type alias Config =
    { reviewDir : String
    , sourceDirs : List String
    , build : String
    , iterations : Int
    }


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.requiredKeywordArg "review-dir"
                        |> Option.withDescription "Path to the elm-review project"
                    )
                |> OptionsParser.with
                    (Option.requiredKeywordArg "source-dirs"
                        |> Option.map (String.split ",")
                        |> Option.withDescription "Comma-separated source directories for the target project"
                    )
                |> OptionsParser.with
                    (Option.requiredKeywordArg "build"
                        |> Option.withDescription "Build directory for benchmark caches"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "iterations"
                        |> Option.map
                            (\maybeIterations ->
                                maybeIterations
                                    |> Maybe.andThen String.toInt
                                    |> Maybe.map (max 1)
                                    |> Maybe.withDefault 3
                            )
                        |> Option.withDescription "How many target-project-runtime iterations to run (default: 3)"
                    )
            )


run : Script
run =
    Script.withCliOptions programConfig task


task : Config -> BackendTask FatalError ()
task config =
    ReviewRunner.benchmarkTargetProjectRuntime
        { reviewDir = config.reviewDir
        , sourceDirs = config.sourceDirs
        , buildDirectory = config.build
        , iterations = config.iterations
        }
        |> BackendTask.andThen Script.log

