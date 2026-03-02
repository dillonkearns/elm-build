module InterpreterTestRunner exposing (run)

{-| A test runner that uses elm-interpreter to evaluate tests in-process.

Instead of compiling with `elm make` and running with `node`, this reads
the test module source and evaluates it directly using the Elm interpreter.
No subprocess invocation — everything happens in pure Elm.

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
import Elm.Syntax.Expression exposing (Expression(..))
import Eval.Module
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import Types


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
    BackendTask.Extra.profiling "interpreter-test-runner" <|
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Reading test source files") <| \_ ->
        Do.allowFatal (File.rawFile "src/SimpleSampleTests.elm") <| \testSource ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Evaluating tests via interpreter") <| \_ ->
        Do.do (BackendTask.Extra.timed "Interpreting tests" "Interpreted tests" (interpretTests testSource)) <| \output ->
        displayResults output


interpretTests : String -> BackendTask FatalError String
interpretTests testSource =
    let
        result : Result Types.Error Types.Value
        result =
            Eval.Module.evalProject
                [ testSource ]
                (FunctionOrValue [] "results")
    in
    case result of
        Ok (Types.String s) ->
            BackendTask.succeed s

        Ok other ->
            BackendTask.fail
                (FatalError.fromString
                    ("Expected String result, got: " ++ Debug.toString other)
                )

        Err (Types.ParsingError deadEnds) ->
            BackendTask.fail
                (FatalError.fromString
                    ("Parsing error: " ++ Debug.toString deadEnds)
                )

        Err (Types.EvalError evalErr) ->
            BackendTask.fail
                (FatalError.fromString
                    ("Eval error: " ++ Debug.toString evalErr.error)
                )


displayResults : String -> BackendTask FatalError ()
displayResults output =
    let
        lines : List String
        lines =
            String.lines output

        summaryLine : String
        summaryLine =
            List.head lines |> Maybe.withDefault ""

        summaryParts : { passed : Int, failed : Int, total : Int }
        summaryParts =
            case String.split "," summaryLine of
                [ p, f, t ] ->
                    { passed = String.toInt p |> Maybe.withDefault 0
                    , failed = String.toInt f |> Maybe.withDefault 0
                    , total = String.toInt t |> Maybe.withDefault 0
                    }

                _ ->
                    { passed = 0, failed = 0, total = 0 }

        testLines : List String
        testLines =
            List.drop 1 lines
                |> List.filter (not << String.isEmpty)

        passLines : List String
        passLines =
            List.filter (String.startsWith "PASS:") testLines

        failLines : List String
        failLines =
            List.filter (String.startsWith "FAIL:") testLines
    in
    Do.each passLines
        (\line ->
            Script.log
                (Ansi.Color.fontColor Ansi.Color.green
                    ("  ✓ " ++ String.dropLeft 5 line)
                )
        )
    <| \_ ->
    Do.each failLines
        (\line ->
            Script.log
                (Ansi.Color.fontColor Ansi.Color.red
                    ("  ✗ " ++ String.dropLeft 5 line)
                )
        )
    <| \_ ->
    Do.log
        (let
            summary =
                "{ \"passed\": "
                    ++ String.fromInt summaryParts.passed
                    ++ ", \"failed\": "
                    ++ String.fromInt summaryParts.failed
                    ++ ", \"total\": "
                    ++ String.fromInt summaryParts.total
                    ++ " }"

            color =
                if summaryParts.failed == 0 then
                    Ansi.Color.fontColor Ansi.Color.brightGreen

                else
                    Ansi.Color.fontColor Ansi.Color.brightRed
         in
         color ("\n" ++ summary)
        )
    <| \_ ->
    Do.noop
