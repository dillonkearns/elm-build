module CoreExtraBenchmark exposing (run)

{-| Benchmark: run elmcraft/core-extra's test suite via the interpreter and
compare against elm-test.

    bunx elm-pages run src/CoreExtraBenchmark.elm -- --build .build/bench

-}

import Ansi.Color
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
                        |> Option.withDescription "Build folder for the benchmark cache"
                    )
            )


coreExtraDir : String
coreExtraDir =
    "/tmp/core-extra"


{-| Packages to skip — they have kernel code the interpreter can't handle.
-}
skipPackages : Set.Set String
skipPackages =
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


{-| Test modules that don't depend on Regex.
-}
testModuleFiles : List String
testModuleFiles =
    [ "ArrayTests.elm"
    , "BasicsTests.elm"
    , "CharTests.elm"
    , "DictTests.elm"
    , "FloatTests.elm"
    , "ListTests.elm"
    , "MaybeTests.elm"
    , "ResultTests.elm"
    , "SetTests.elm"
    , "TripleTests.elm"
    , "Utils.elm"
    ]


task : Config -> BackendTask FatalError ()
task config =
    Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "\n=== Core-Extra Benchmark ===\n") <| \_ ->
    -- Step 1: Load package dependencies (with skips)
    Do.do
        (BackendTask.Extra.timed "Loading package deps" "Loaded package deps"
            (ProjectSources.loadPackageDeps
                { projectDir = Path.path "."
                , skipPackages = skipPackages
                }
            )
        )
    <| \packageSources ->
    -- Step 2: Load test files from /tmp/core-extra/tests/
    Do.do
        (BackendTask.Extra.timed "Loading test files" "Loaded test files"
            (loadTestFiles coreExtraDir)
        )
    <| \testFiles ->
    -- Step 3: Load SimpleTestRunner source
    Do.allowFatal (File.rawFile "src/SimpleTestRunner.elm") <| \simpleTestRunnerSource ->
    let
        -- Step 4: Patch sources for interpreter compatibility
        patchedPackageSources : List String
        patchedPackageSources =
            List.map patchSource packageSources

        -- Step 5: Create wrapper module
        wrapperModule : String
        wrapperModule =
            buildWrapperModule

        -- Step 6: Assemble all sources
        allSources : List String
        allSources =
            patchedPackageSources ++ testFiles ++ [ simpleTestRunnerSource, wrapperModule ]
    in
    Do.log
        (Ansi.Color.fontColor Ansi.Color.brightBlue
            ("Loaded " ++ String.fromInt (List.length allSources) ++ " source files")
        )
    <| \_ ->
    -- Step 7: Run interpreter and time it
    Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Running tests via interpreter...") <| \_ ->
    Do.do BackendTask.Time.now <| \interpStart ->
    Do.do (interpretTests allSources) <| \output ->
    Do.do BackendTask.Time.now <| \interpEnd ->
    let
        interpMs : Int
        interpMs =
            Time.posixToMillis interpEnd - Time.posixToMillis interpStart
    in
    Do.do (displayResults output) <| \_ ->
    Do.log
        (Ansi.Color.fontColor Ansi.Color.brightCyan
            ("\nInterpreter time: " ++ String.fromInt interpMs ++ "ms")
        )
    <| \_ ->
    -- Step 8: Run elm-test for comparison
    Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "\nRunning elm-test for comparison...") <| \_ ->
    Do.do BackendTask.Time.now <| \elmTestStart ->
    Do.do (runElmTest |> BackendTask.toResult) <| \elmTestResult ->
    Do.do BackendTask.Time.now <| \elmTestEnd ->
    let
        elmTestMs : Int
        elmTestMs =
            Time.posixToMillis elmTestEnd - Time.posixToMillis elmTestStart

        elmTestSummary : String
        elmTestSummary =
            case elmTestResult of
                Ok _ ->
                    "elm-test passed"

                Err _ ->
                    "elm-test completed (may have failures)"
    in
    Do.log
        (Ansi.Color.fontColor Ansi.Color.brightCyan
            ("elm-test time: " ++ String.fromInt elmTestMs ++ "ms (" ++ elmTestSummary ++ ")")
        )
    <| \_ ->
    Do.log
        (Ansi.Color.fontColor Ansi.Color.brightYellow
            ("\n=== Summary ===\n"
                ++ "Interpreter: "
                ++ String.fromInt interpMs
                ++ "ms\n"
                ++ "elm-test:    "
                ++ String.fromInt elmTestMs
                ++ "ms\n"
                ++ "Ratio:       "
                ++ formatFloat (toFloat interpMs / toFloat (max 1 elmTestMs))
                ++ "x"
            )
        )
    <| \_ ->
    Do.noop


loadTestFiles : String -> BackendTask FatalError (List String)
loadTestFiles projectDir =
    testModuleFiles
        |> List.map
            (\name ->
                File.rawFile (projectDir ++ "/tests/" ++ name)
                    |> BackendTask.allowFatal
            )
        |> BackendTask.Extra.combine


{-| Patch source files for interpreter compatibility:

1.  Replace `Elm.Kernel.Test.runThunk` with a pure Elm equivalent
2.  Fix aliased Micro\*Extra imports (the interpreter incorrectly resolves
    `List.map` to `MicroListExtra.map` when `import MicroListExtra as List`
    is present). We remove the alias and qualify Micro\*-specific function
    calls with their full module name.

-}
patchSource : String -> String
patchSource source =
    source
        |> patchRunThunk
        |> patchMicroListExtra
        |> patchMicroDictExtra
        |> patchMicroMaybeExtra
        |> patchMicroBitwiseExtra


patchRunThunk : String -> String
patchRunThunk source =
    if String.contains "runThunk =\n    Elm.Kernel.Test.runThunk" source then
        source
            |> String.replace
                "runThunk =\n    Elm.Kernel.Test.runThunk"
                "runThunk fn =\n    Ok (fn ())"

    else
        source


patchMicroListExtra : String -> String
patchMicroListExtra source =
    if String.contains "import MicroListExtra as List" source then
        source
            |> String.replace "import MicroListExtra as List" "import MicroListExtra"
            |> String.replace "List.fastConcat " "MicroListExtra.fastConcat "
            |> String.replace "List.fastConcat\n" "MicroListExtra.fastConcat\n"
            |> String.replace "List.fastConcatMap " "MicroListExtra.fastConcatMap "
            |> String.replace "List.fastConcatMap\n" "MicroListExtra.fastConcatMap\n"
            |> String.replace "List.find " "MicroListExtra.find "
            |> String.replace "List.find\n" "MicroListExtra.find\n"
            |> String.replace "List.getAt " "MicroListExtra.getAt "
            |> String.replace "List.getAt\n" "MicroListExtra.getAt\n"
            |> String.replace "List.setAt " "MicroListExtra.setAt "
            |> String.replace "List.setAt\n" "MicroListExtra.setAt\n"
            |> String.replace "List.splitWhen " "MicroListExtra.splitWhen "
            |> String.replace "List.splitWhen\n" "MicroListExtra.splitWhen\n"
            |> String.replace "List.transpose " "MicroListExtra.transpose "
            |> String.replace "List.transpose\n" "MicroListExtra.transpose\n"
            |> String.replace "List.unique " "MicroListExtra.unique "
            |> String.replace "List.unique\n" "MicroListExtra.unique\n"

    else
        source


patchMicroDictExtra : String -> String
patchMicroDictExtra source =
    if String.contains "import MicroDictExtra as Dict" source then
        source
            |> String.replace "import MicroDictExtra as Dict" "import MicroDictExtra"
            |> String.replace "Dict.any " "MicroDictExtra.any "
            |> String.replace "Dict.any\n" "MicroDictExtra.any\n"
            |> String.replace "Dict.increment " "MicroDictExtra.increment "
            |> String.replace "Dict.increment\n" "MicroDictExtra.increment\n"

    else
        source


patchMicroMaybeExtra : String -> String
patchMicroMaybeExtra source =
    if String.contains "import MicroMaybeExtra as Maybe" source then
        source
            |> String.replace "import MicroMaybeExtra as Maybe" "import MicroMaybeExtra"
            |> String.replace "Maybe.traverse " "MicroMaybeExtra.traverse "
            |> String.replace "Maybe.traverse\n" "MicroMaybeExtra.traverse\n"

    else
        source


patchMicroBitwiseExtra : String -> String
patchMicroBitwiseExtra source =
    if String.contains "import MicroBitwiseExtra as Bitwise" source then
        source
            |> String.replace "import MicroBitwiseExtra as Bitwise" "import MicroBitwiseExtra"
            |> String.replace "Bitwise.int52FromTuple " "MicroBitwiseExtra.int52FromTuple "
            |> String.replace "Bitwise.int52FromTuple\n" "MicroBitwiseExtra.int52FromTuple\n"
            |> String.replace "Bitwise.int52ToTuple " "MicroBitwiseExtra.int52ToTuple "
            |> String.replace "Bitwise.int52ToTuple\n" "MicroBitwiseExtra.int52ToTuple\n"
            |> String.replace "Bitwise.isBitSet " "MicroBitwiseExtra.isBitSet "
            |> String.replace "Bitwise.isBitSet\n" "MicroBitwiseExtra.isBitSet\n"
            |> String.replace "Bitwise.keepBits " "MicroBitwiseExtra.keepBits "
            |> String.replace "Bitwise.keepBits\n" "MicroBitwiseExtra.keepBits\n"
            |> String.replace "Bitwise.reverse52Bits " "MicroBitwiseExtra.reverse52Bits "
            |> String.replace "Bitwise.reverse52Bits\n" "MicroBitwiseExtra.reverse52Bits\n"
            |> String.replace "Bitwise.reverseNBits " "MicroBitwiseExtra.reverseNBits "
            |> String.replace "Bitwise.reverseNBits\n" "MicroBitwiseExtra.reverseNBits\n"
            |> String.replace "Bitwise.signedToUnsigned " "MicroBitwiseExtra.signedToUnsigned "
            |> String.replace "Bitwise.signedToUnsigned\n" "MicroBitwiseExtra.signedToUnsigned\n"

    else
        source


{-| Build the wrapper module that imports all test modules and runs them.
-}
buildWrapperModule : String
buildWrapperModule =
    let
        imports : String
        imports =
            testModuleImports
                |> List.map (\m -> "import " ++ m)
                |> String.join "\n"

        testList : String
        testList =
            testModuleSuites
                |> List.map (\s -> "            , " ++ s)
                |> String.join "\n"
                |> String.dropLeft 14
    in
    "module CoreExtraAllTests exposing (results)\n\n"
        ++ "import SimpleTestRunner\n"
        ++ "import Test exposing (Test, describe)\n"
        ++ imports
        ++ "\n\n\nresults : String\nresults =\n"
        ++ "    SimpleTestRunner.runToString\n"
        ++ "        (describe \"core-extra\"\n"
        ++ "            [ "
        ++ testList
        ++ "\n            ]\n        )\n"


{-| Module names to import in the wrapper.

Tests we can run. Modules that use Fuzz.string, Fuzz.intRange with large
ranges, or Fuzz.array trigger an interpreter bug with partial application of
point-free kernel functions in Random.int's non-power-of-2 path.

Working: BasicsTests, CharTests, MaybeTests, TripleTests (51 tests)
Blocked: ArrayTests, DictTests, FloatTests, ListTests, ResultTests, SetTests

-}
testModuleImports : List String
testModuleImports =
    [ "BasicsTests"
    , "CharTests"
    , "MaybeTests"
    , "TripleTests"
    ]


{-| Suite expressions for each test module.
-}
testModuleSuites : List String
testModuleSuites =
    [ "BasicsTests.suite"
    , "CharTests.suite"
    , "MaybeTests.suite"
    , "TripleTests.suite"
    ]


interpretTests : List String -> BackendTask FatalError String
interpretTests allSources =
    let
        result : Result Types.Error Types.Value
        result =
            Eval.Module.evalProject
                allSources
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


runElmTest : BackendTask FatalError ()
runElmTest =
    Script.exec "bash"
        [ "-c"
        , "cd " ++ coreExtraDir ++ " && npx elm-test"
        ]


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

        failLines : List String
        failLines =
            List.filter (String.startsWith "FAIL:") testLines
    in
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
         color summary
        )
    <| \_ ->
    Do.noop


formatFloat : Float -> String
formatFloat f =
    let
        whole =
            floor f

        frac =
            round ((f - toFloat whole) * 100)

        fracStr =
            if frac < 10 then
                "0" ++ String.fromInt frac

            else
                String.fromInt frac
    in
    String.fromInt whole ++ "." ++ fracStr
