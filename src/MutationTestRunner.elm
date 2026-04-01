module MutationTestRunner exposing (run)

{-| Mutation testing runner that uses the elm-interpreter via InterpreterProject.

For each mutation, swaps the mutated source into the interpreter evaluation
via `evalWithSourceOverrides`. Results are cached via elm-build's
content-addressed Cache, so re-runs with unchanged code are instant.

The test module just needs to expose `suite : Test` — the standard elm-test
convention. Test values are auto-discovered via the interpreter.

-}

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Glob as Glob
import BackendTask.Time
import Cache
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import DepGraph
import Elm.Syntax.Expression exposing (Expression(..))
import Eval.Module
import FatalError exposing (FatalError)
import InterpreterProject exposing (InterpreterProject)
import Mutator exposing (Mutation)
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import Set
import TestAnalysis
import Time
import Types


type alias Config =
    { mutateFile : String
    , testFile : Maybe String
    , suiteName : String
    , sourceDirs : List String
    , buildDirectory : Path
    , verbose : Bool
    }


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.optionalKeywordArg "mutate"
                        |> Option.map (Maybe.withDefault "src/MathLib.elm")
                        |> Option.withDescription "The source file to mutate"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "test"
                        |> Option.withDescription "The test file (auto-discovered if omitted)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "suite"
                        |> Option.map (Maybe.withDefault "suite")
                        |> Option.withDescription "The name of the test value (default: suite)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "source-dirs"
                        |> Option.map (Maybe.map (String.split ",") >> Maybe.withDefault [])
                        |> Option.withDescription "Extra source directories (comma-separated)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "build"
                        |> Option.map (Maybe.withDefault ".elm-mutation-test" >> Path.path)
                        |> Option.withDescription "Build/cache directory (default: .elm-mutation-test)"
                    )
                |> OptionsParser.with
                    (Option.flag "verbose"
                        |> Option.withDescription "Show killed mutants (default: only show survivors)"
                    )
            )


run : Script
run =
    Script.withCliOptions programConfig task


task : Config -> BackendTask FatalError ()
task config =
    BackendTask.Extra.profiling "mutation-test-runner" <|
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Loading project sources for mutation testing") <| \_ ->
        -- Resolve the test file: either explicitly provided or auto-discovered
        Do.do (resolveTestFile config) <| \testFile ->
        Do.allowFatal (File.rawFile testFile) <| \testSource ->
        let
            testModuleName : String
            testModuleName =
                DepGraph.parseModuleName testSource
                    |> Maybe.withDefault "Tests"

            candidateNames : List String
            candidateNames =
                TestAnalysis.getCandidateNames testSource
        in
        Do.log ("Found " ++ String.fromInt (List.length candidateNames) ++ " candidate values in " ++ testModuleName) <| \_ ->
        -- Load the project via InterpreterProject (packages parsed once, cached)
        Do.do
            (BackendTask.Extra.timed "Loading project" "Loaded project"
                (InterpreterProject.loadWith
                    { projectDir = Path.path "."
                    , skipPackages = kernelPackages
                    , patchSource = patchSource
                    , extraSourceFiles = []
                    , sourceDirectories = Just ("src" :: "tests" :: config.sourceDirs)
                    }
                )
            )
        <| \project ->
        -- Discover which candidates are actually Test values via the interpreter
        let
            packageEnv =
                InterpreterProject.getPackageEnv project

            -- Get user sources needed for probing (test module + its deps)
            probeSources =
                let
                    { userSources } =
                        InterpreterProject.prepareEvalSources project
                            { imports = [ "SimpleTestRunner", testModuleName ]
                            , expression = "\"probe\""
                            }
                in
                simpleTestRunnerSource :: userSources
        in
        Do.do
            (BackendTask.Extra.timed "Discovering test values" "Discovered test values"
                (let
                    probeResults =
                        candidateNames
                            |> List.map
                                (\name ->
                                    ( name, TestAnalysis.probeCandidate packageEnv testModuleName name probeSources )
                                )

                    testValues =
                        probeResults
                            |> List.filterMap
                                (\( name, result ) ->
                                    case result of
                                        Ok _ ->
                                            Just name

                                        Err _ ->
                                            Nothing
                                )

                    rejections =
                        probeResults
                            |> List.filterMap
                                (\( name, result ) ->
                                    case result of
                                        Err reason ->
                                            Just (testModuleName ++ "." ++ name ++ ": " ++ reason)

                                        Ok _ ->
                                            Nothing
                                )
                 in
                 if List.isEmpty testValues then
                    BackendTask.fail
                        (FatalError.fromString
                            ("No Test values found in "
                                ++ testModuleName
                                ++ ".\nCandidates rejected:\n  "
                                ++ String.join "\n  " rejections
                            )
                        )

                 else
                    (if List.isEmpty rejections then
                        BackendTask.succeed ()

                     else
                        Script.log ("Rejected candidates: " ++ String.join ", " rejections)
                    )
                        |> BackendTask.map (\() -> testValues)
                )
            )
        <| \testValues ->
        let
            suiteExpr : String
            suiteExpr =
                case testValues of
                    [ single ] ->
                        testModuleName ++ "." ++ single

                    multiple ->
                        "Test.describe \"" ++ testModuleName ++ "\" [" ++ String.join ", " (List.map (\v -> testModuleName ++ "." ++ v) multiple) ++ "]"

            evalConfig : { imports : List String, expression : String }
            evalConfig =
                { imports = [ "SimpleTestRunner", "Test", testModuleName ]
                , expression = "SimpleTestRunner.runToString (" ++ suiteExpr ++ ")"
                }
        in
        Do.log ("Discovered test values: " ++ String.join ", " (List.map (\v -> testModuleName ++ "." ++ v) testValues)) <| \_ ->
        Do.allowFatal (File.rawFile config.mutateFile) <| \mutateSource ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue ("Generating mutations for " ++ config.mutateFile)) <| \_ ->
        let
            mutations : List Mutation
            mutations =
                Mutator.generateMutations mutateSource

            mutationCount : Int
            mutationCount =
                List.length mutations
        in
        Do.log ("Found " ++ String.fromInt mutationCount ++ " mutations") <| \_ ->
        Do.exec "mkdir" [ "-p", Path.toString config.buildDirectory ] <| \_ ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Running mutation tests via interpreter (cached)") <| \_ ->
        Do.do BackendTask.Time.now <| \startTime ->
        -- Run each mutation through Cache (independently cached)
        Do.do
            (mutations
                |> List.map
                    (\mutation ->
                        Do.do
                            (Cache.run { jobs = Nothing } config.buildDirectory
                                (InterpreterProject.evalWithSourceOverrides project
                                    { imports = evalConfig.imports
                                    , expression = evalConfig.expression
                                    , sourceOverrides = [ simpleTestRunnerSource, mutation.mutatedSource ]
                                    }
                                    Cache.succeed
                                )
                            )
                        <| \result ->
                        Do.allowFatal (File.rawFile (Path.toString result.output)) <| \output ->
                        BackendTask.succeed (parseMutationResult mutation output)
                    )
                |> BackendTask.Extra.sequence
            )
        <| \results ->
        Do.do BackendTask.Time.now <| \endTime ->
        let
            _ = results

            evalMs =
                Time.posixToMillis endTime - Time.posixToMillis startTime

            perMutation =
                if mutationCount > 0 then
                    evalMs // mutationCount

                else
                    0
        in
        Do.log
            ("Evaluated "
                ++ String.fromInt mutationCount
                ++ " mutations in "
                ++ String.fromInt evalMs
                ++ "ms ("
                ++ String.fromInt perMutation
                ++ "ms/mutation)"
            )
        <| \_ ->
        displayMutationReport config.verbose mutateSource config.mutateFile results


parseMutationResult : Mutation -> String -> MutationResult
parseMutationResult mutation output =
    if String.startsWith "ERROR:" output then
        ErrorResult { mutation = mutation, error = String.dropLeft 7 output }

    else
        case String.split "," (String.lines output |> List.head |> Maybe.withDefault "") of
            [ _, failStr, _ ] ->
                case String.toInt failStr of
                    Just failCount ->
                        if failCount > 0 then
                            Killed { mutation = mutation, failCount = failCount }

                        else
                            Survived { mutation = mutation }

                    Nothing ->
                        ErrorResult { mutation = mutation, error = "Could not parse fail count" }

            _ ->
                ErrorResult { mutation = mutation, error = "Unexpected output format" }


type MutationResult
    = Killed { mutation : Mutation, failCount : Int }
    | Survived { mutation : Mutation }
    | ErrorResult { mutation : Mutation, error : String }


displayMutationReport : Bool -> String -> String -> List MutationResult -> BackendTask FatalError ()
displayMutationReport verbose sourceCode filePath results =
    let
        killed =
            List.filter isKilled results

        survived =
            List.filter isSurvived results

        errors =
            List.filter isError results

        total =
            List.length results

        score =
            if total == 0 then
                "N/A"

            else
                let
                    pct =
                        toFloat (List.length killed) / toFloat total * 100
                in
                String.fromInt (round pct) ++ "%"
    in
    Do.do
        (if verbose then
            Do.each killed
                (\r ->
                    case r of
                        Killed { mutation, failCount } ->
                            Script.log
                                (Ansi.Color.fontColor Ansi.Color.green
                                    ("  ✓ Killed (" ++ String.fromInt failCount ++ " failed): " ++ mutation.description ++ " (line " ++ String.fromInt mutation.line ++ ")")
                                )

                        _ ->
                            Script.log ""
                )
                (\_ -> BackendTask.succeed ())

         else
            BackendTask.succeed ()
        )
    <| \_ ->
    Do.each survived
        (\r ->
            case r of
                Survived { mutation } ->
                    Script.log
                        (Ansi.Color.fontColor Ansi.Color.red
                            ("  ✗ Survived: "
                                ++ mutation.description
                                ++ " ("
                                ++ filePath
                                ++ ":"
                                ++ String.fromInt mutation.line
                                ++ ")"
                            )
                            ++ "\n"
                            ++ sourceContext sourceCode mutation.line
                        )

                _ ->
                    Script.log ""
        )
    <| \_ ->
    Do.each errors
        (\r ->
            case r of
                ErrorResult { mutation, error } ->
                    Script.log
                        (Ansi.Color.fontColor Ansi.Color.yellow
                            ("  ? Error: " ++ mutation.description ++ " — " ++ error)
                        )

                _ ->
                    Script.log ""
        )
    <| \_ ->
    let
        summaryColor =
            if List.isEmpty survived then
                Ansi.Color.fontColor Ansi.Color.brightGreen

            else
                Ansi.Color.fontColor Ansi.Color.brightRed
    in
    Do.log
        (summaryColor
            ("\nMutation Score: "
                ++ score
                ++ " ("
                ++ String.fromInt (List.length killed)
                ++ " killed, "
                ++ String.fromInt (List.length survived)
                ++ " survived, "
                ++ String.fromInt (List.length errors)
                ++ " errors, "
                ++ String.fromInt total
                ++ " total)"
            )
        )
    <| \_ ->
    if List.isEmpty survived && List.isEmpty errors then
        Do.noop

    else
        BackendTask.fail
            (FatalError.build
                { title = "Mutation testing incomplete"
                , body =
                    String.fromInt (List.length survived)
                        ++ " surviving mutant(s)"
                        ++ (if List.isEmpty errors then
                                ""

                            else
                                ", " ++ String.fromInt (List.length errors) ++ " error(s)"
                           )
                }
            )


isKilled : MutationResult -> Bool
isKilled r =
    case r of
        Killed _ ->
            True

        _ ->
            False


isSurvived : MutationResult -> Bool
isSurvived r =
    case r of
        Survived _ ->
            True

        _ ->
            False


isError : MutationResult -> Bool
isError r =
    case r of
        ErrorResult _ ->
            True

        _ ->
            False


{-| The SimpleTestRunner module source, embedded so it's available regardless
of what project directory the tool runs from.
-}
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


{-| Resolve the test file: use --test if provided, otherwise auto-discover
by finding test files that import the mutated module.
-}
resolveTestFile : Config -> BackendTask FatalError String
resolveTestFile config =
    case config.testFile of
        Just explicit ->
            Do.log ("Using test file: " ++ explicit) <| \_ ->
            BackendTask.succeed explicit

        Nothing ->
            Do.allowFatal (File.rawFile config.mutateFile) <| \mutateSource ->
            let
                moduleName =
                    DepGraph.parseModuleName mutateSource
                        |> Maybe.withDefault ""
            in
            Do.log ("Auto-discovering test files that import " ++ moduleName ++ "...") <| \_ ->
            Do.do (findTestFilesImporting moduleName) <| \testFiles ->
            case testFiles of
                [ single ] ->
                    Do.log ("Found test file: " ++ single) <| \_ ->
                    BackendTask.succeed single

                first :: _ ->
                    Do.log ("Found " ++ String.fromInt (List.length testFiles) ++ " test files: " ++ String.join ", " testFiles ++ " (using first)") <| \_ ->
                    BackendTask.succeed first

                [] ->
                    BackendTask.fail
                        (FatalError.fromString
                            ("No test files found that import "
                                ++ moduleName
                                ++ ". Looked in tests/**/*.elm. Use --test to specify manually."
                            )
                        )


findTestFilesImporting : String -> BackendTask FatalError (List String)
findTestFilesImporting moduleName =
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
                                        List.member moduleName (DepGraph.parseImports content)
                                    )
                                |> List.map Tuple.first
                        )
            )


sourceContext : String -> Int -> String
sourceContext source line =
    let
        lines =
            String.lines source

        getLine n =
            lines |> List.drop (n - 1) |> List.head |> Maybe.withDefault ""

        pad n =
            let
                s =
                    String.fromInt n
            in
            String.repeat (4 - String.length s) " " ++ s

        showLine n =
            if n >= 1 && n <= List.length lines then
                let
                    prefix =
                        if n == line then
                            Ansi.Color.fontColor Ansi.Color.red ("  > " ++ pad n ++ " | ")

                        else
                            "    " ++ pad n ++ " | "
                in
                prefix ++ getLine n

            else
                ""
    in
    [ showLine (line - 1), showLine line, showLine (line + 1) ]
        |> List.filter (not << String.isEmpty)
        |> String.join "\n"


kernelPackages : Set.Set String
kernelPackages =
    Set.fromList
        [ "elm/json"
        , "elm/regex"
        , "elm/html"
        , "elm/virtual-dom"
        , "elm/bytes"
        , "elm/browser"
        , "elm/http"
        , "elm/file"
        , "elm/url"
        ]


patchSource : String -> String
patchSource source =
    if String.contains "runThunk =\n    Elm.Kernel.Test.runThunk" source then
        source
            |> String.replace
                "runThunk =\n    Elm.Kernel.Test.runThunk"
                "runThunk fn =\n    Ok (fn ())"

    else
        source
