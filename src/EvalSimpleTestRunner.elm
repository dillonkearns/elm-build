module EvalSimpleTestRunner exposing (run)

{-| Runs SimpleSampleTests via evalWith (elm make + node) for fair comparison
with the interpreter approach.
-}

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import Cache
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import ElmProject
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Path exposing (Path)


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
    BackendTask.Extra.profiling "eval-simple-test-runner" <|
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Hashing source files and building dependency graph") <| \_ ->
        Do.do (ElmProject.fromPath (Path.path ".")) <| \project ->
        Do.exec "mkdir" [ "-p", Path.toString config.buildDirectory ] <| \_ ->
        Do.do
            (Cache.run { jobs = Nothing } config.buildDirectory
                (ElmProject.evalWith project
                    { imports = [ "SimpleSampleTests" ]
                    , expression = "SimpleSampleTests.results"
                    }
                    Cache.succeed
                    |> Cache.timed "Running tests" "Ran tests"
                )
            )
        <| \result ->
        Do.allowFatal (File.rawFile (Path.toString result.output)) <| \output ->
        Script.log output
