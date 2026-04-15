module TestRunner exposing (run)

{-| Run an Elm test suite via the interpreter.

Usage:
npx elm-pages run src/TestRunner.elm --test tests/MyTests.elm

Or with no args to auto-discover test files.

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
import FatalError exposing (FatalError)
import InterpreterProject exposing (InterpreterProject)
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import Set
import TestAnalysis
import Time
import Types exposing (Value(..))


type alias Config =
    { testFile : Maybe String
    , sourceDirs : List String
    , buildDirectory : Path
    }


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.optionalKeywordArg "test"
                        |> Option.withDescription "Test file(s), comma-separated (auto-discovered if omitted)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "source-dirs"
                        |> Option.map (Maybe.map (String.split ",") >> Maybe.withDefault [])
                        |> Option.withDescription "Extra source directories (comma-separated)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "build"
                        |> Option.map (Maybe.withDefault ".elm-build" >> Path.path)
                        |> Option.withDescription "Build/cache directory (default: .elm-build)"
                    )
            )


run : Script
run =
    Script.withCliOptions programConfig (task >> BackendTask.quiet)


task : Config -> BackendTask FatalError ()
task config =
    Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Running tests via interpreter") <|
        \_ ->
            Do.do ensureDeps <|
                \_ ->
                    let
                        sourceDirectories =
                            "src" :: config.sourceDirs

                        allDirectories =
                            "tests" :: sourceDirectories
                    in
                    -- Resolve the test files BEFORE loading the project so we can pass
                    -- their module names as normalization roots. The loader uses the
                    -- roots to compute the transitive import closure and only
                    -- normalizes user modules that are actually reachable from a test.
                    -- Modules outside the closure (e.g. `String.Diacritics` when running
                    -- `tests/BasicsTests.elm`) skip the eager fixpoint eval entirely.
                    Do.do (resolveTestFiles config) <|
                        \testFiles ->
                            Do.do (resolveTestModuleNames testFiles) <|
                                \testModuleNames ->
                                    Do.do
                                        (BackendTask.Extra.timed "Loading project"
                                            "Loaded project"
                                            (InterpreterProject.loadWith
                                                { projectDir = Path.path "."
                                                , skipPackages = kernelPackages
                                                , patchSource = patchSource
                                                , patchUserSource = \_ source -> source
                                                , extraSourceFiles = []
                                                , extraReachableImports = [ "Test", "Fuzz", "Expect", "Test.Runner" ]
                                                , sourceDirectories = Just allDirectories
                                                , normalizationRoots = Just testModuleNames
                                                }
                                            )
                                        )
                                    <|
                                        \project ->
                                            Do.log ("Found " ++ String.fromInt (List.length testFiles) ++ " test file(s)") <|
                                                \_ ->
                                                    Do.do BackendTask.Time.now <|
                                                        \startTime ->
                                                            -- Run each test file
                                                            Do.do
                                                                (testFiles
                                                                    |> List.map (\testFile -> runTestFile config project testFile)
                                                                    |> BackendTask.Extra.sequence
                                                                )
                                                            <|
                                                                \allResults ->
                                                                    Do.do BackendTask.Time.now <|
                                                                        \endTime ->
                                                                            let
                                                                                totalPassed =
                                                                                    List.sum (List.map .passed allResults)

                                                                                totalFailed =
                                                                                    List.sum (List.map .failed allResults)

                                                                                totalSkipped =
                                                                                    List.sum (List.map .skipped allResults)

                                                                                evalMs =
                                                                                    Time.posixToMillis endTime - Time.posixToMillis startTime

                                                                                summaryColor =
                                                                                    if totalFailed == 0 && totalSkipped == 0 then
                                                                                        Ansi.Color.fontColor Ansi.Color.brightGreen

                                                                                    else if totalFailed > 0 then
                                                                                        Ansi.Color.fontColor Ansi.Color.brightRed

                                                                                    else
                                                                                        Ansi.Color.fontColor Ansi.Color.yellow
                                                                            in
                                                                            Do.log
                                                                                (summaryColor
                                                                                    ("\nDuration: "
                                                                                        ++ String.fromInt evalMs
                                                                                        ++ "ms\nPassed:   "
                                                                                        ++ String.fromInt totalPassed
                                                                                        ++ "\nFailed:   "
                                                                                        ++ String.fromInt totalFailed
                                                                                        ++ (if totalSkipped > 0 then
                                                                                                "\nSkipped:  " ++ String.fromInt totalSkipped ++ " (interpreter limitations)"

                                                                                            else
                                                                                                ""
                                                                                           )
                                                                                    )
                                                                                )
                                                                            <|
                                                                                \_ ->
                                                                                    if totalFailed > 0 then
                                                                                        BackendTask.fail
                                                                                            (FatalError.build
                                                                                                { title = "Tests Failed"
                                                                                                , body = String.fromInt totalFailed ++ " tests failed"
                                                                                                }
                                                                                            )

                                                                                    else
                                                                                        Do.noop


type alias TestFileResult =
    { file : String
    , passed : Int
    , failed : Int
    , skipped : Int
    }


runTestFile : Config -> InterpreterProject -> String -> BackendTask FatalError TestFileResult
runTestFile config project testFile =
    Do.allowFatal (File.rawFile testFile) <|
        \testSource ->
            let
                testModuleName =
                    DepGraph.parseModuleName testSource |> Maybe.withDefault "Tests"

                -- Fast path: use type annotations to discover Test values (no eval needed)
                staticTestValues =
                    TestAnalysis.discoverTestValues testSource

                -- Slow path: probe all zero-arg exposed values via interpreter
                probeTestValues () =
                    let
                        candidateNames =
                            TestAnalysis.getCandidateNames testSource

                        packageEnv =
                            InterpreterProject.getPackageEnv project

                        probeSources =
                            let
                                { userSources } =
                                    InterpreterProject.prepareEvalSources project
                                        { imports = [ "SimpleTestRunner", testModuleName ]
                                        , expression = "\"probe\""
                                        }
                            in
                            simpleTestRunnerSource :: userSources

                        probeResults =
                            candidateNames
                                |> List.map
                                    (\name ->
                                        ( name, TestAnalysis.probeCandidate packageEnv testModuleName name probeSources )
                                    )
                    in
                    { testValues =
                        probeResults
                            |> List.filterMap
                                (\( name, result ) ->
                                    case result of
                                        Ok _ ->
                                            Just name

                                        Err _ ->
                                            Nothing
                                )
                    , rejections =
                        probeResults
                            |> List.filterMap
                                (\( name, result ) ->
                                    case result of
                                        Err reason ->
                                            Just (name ++ ": " ++ reason)

                                        Ok _ ->
                                            Nothing
                                )
                            |> String.join "; "
                    , candidateCount = List.length candidateNames
                    }

                { testValues, rejections, candidateCount } =
                    if List.isEmpty staticTestValues then
                        -- No type annotations found — fall back to probing
                        probeTestValues ()

                    else
                        { testValues = staticTestValues
                        , rejections = ""
                        , candidateCount = List.length staticTestValues
                        }
            in
            if List.isEmpty testValues then
                -- Can't run this test file (interpreter limitation)
                Do.log (Ansi.Color.fontColor Ansi.Color.yellow ("  ⊘ " ++ testModuleName ++ " (skipped: " ++ rejections ++ ")")) <|
                    \_ ->
                        BackendTask.succeed { file = testFile, passed = 0, failed = 0, skipped = candidateCount }

            else
                let
                    suiteExpr =
                        case testValues of
                            [ single ] ->
                                testModuleName ++ "." ++ single

                            multiple ->
                                "Test.describe \"" ++ testModuleName ++ "\" [" ++ String.join ", " (List.map (\v -> testModuleName ++ "." ++ v) multiple) ++ "]"

                    evalExpression =
                        "SimpleTestRunner.runToString (" ++ suiteExpr ++ ")"
                in
                Do.do
                    (Cache.run { jobs = Nothing }
                        config.buildDirectory
                        (InterpreterProject.evalWithFileOverrides project
                            { imports = [ "SimpleTestRunner", "Test", testModuleName ]
                            , expression = evalExpression
                            , sourceOverrides = [ simpleTestRunnerSource ]
                            , fileOverrides = []
                            }
                            Cache.succeed
                        )
                    )
                <|
                    \cacheResult ->
                        Do.allowFatal (File.rawFile (Path.toString cacheResult.output)) <|
                            \output ->
                                if String.startsWith "ERROR:" output then
                                    Do.log (Ansi.Color.fontColor Ansi.Color.yellow ("  ⊘ " ++ testModuleName ++ " (error: " ++ String.left 500 output ++ ")")) <|
                                        \_ ->
                                            BackendTask.succeed { file = testFile, passed = 0, failed = 0, skipped = 1 }

                                else
                                    case String.split "," (String.lines output |> List.head |> Maybe.withDefault "") of
                                        [ passStr, failStr, _ ] ->
                                            let
                                                passCount =
                                                    String.toInt passStr |> Maybe.withDefault 0

                                                failCount =
                                                    String.toInt failStr |> Maybe.withDefault 0

                                                failures =
                                                    String.lines output
                                                        |> List.drop 1
                                                        |> List.filter (String.startsWith "FAIL:")
                                            in
                                            Do.do
                                                (if failCount == 0 then
                                                    Script.log (Ansi.Color.fontColor Ansi.Color.green ("  ✓ " ++ testModuleName ++ " (" ++ String.fromInt passCount ++ " passed)"))

                                                 else
                                                    Do.do
                                                        (Script.log (Ansi.Color.fontColor Ansi.Color.red ("  ✗ " ++ testModuleName ++ " (" ++ String.fromInt failCount ++ " failed, " ++ String.fromInt passCount ++ " passed)")))
                                                    <|
                                                        \_ ->
                                                            Do.each failures
                                                                (\line ->
                                                                    Script.log (Ansi.Color.fontColor Ansi.Color.red ("      " ++ line))
                                                                )
                                                                (\_ -> BackendTask.succeed ())
                                                )
                                            <|
                                                \_ ->
                                                    BackendTask.succeed { file = testFile, passed = passCount, failed = failCount, skipped = 0 }

                                        _ ->
                                            Do.log (Ansi.Color.fontColor Ansi.Color.yellow ("  ⊘ " ++ testModuleName ++ " (unexpected output)")) <|
                                                \_ ->
                                                    BackendTask.succeed { file = testFile, passed = 0, failed = 0, skipped = 1 }


{-| Discover test files: use --test if provided, otherwise find all tests/\*\*/\*.elm that import Test.
-}
resolveTestModuleNames : List String -> BackendTask FatalError (List String)
resolveTestModuleNames testFiles =
    testFiles
        |> List.map
            (\filePath ->
                File.rawFile filePath
                    |> BackendTask.allowFatal
                    |> BackendTask.map
                        (\content ->
                            DepGraph.parseModuleName content
                        )
            )
        |> BackendTask.sequence
        |> BackendTask.map (List.filterMap identity)


resolveTestFiles : Config -> BackendTask FatalError (List String)
resolveTestFiles config =
    case config.testFile of
        Just explicit ->
            BackendTask.succeed (String.split "," explicit)

        Nothing ->
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
                                                List.member "Test" (DepGraph.parseImports content)
                                            )
                                        |> List.map Tuple.first
                                        |> List.sort
                                )
                    )


ensureDeps : BackendTask FatalError ()
ensureDeps =
    File.rawFile "elm.json"
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\raw ->
                let
                    isPackage =
                        String.contains "\"type\": \"package\"" raw
                            || String.contains "\"type\":\"package\"" raw

                    fetchCmd =
                        if isPackage then
                            Script.exec "elm" [ "make", "--docs", "/tmp/elm-test-runner-docs.json" ]

                        else
                            Glob.fromStringWithOptions
                                (let
                                    o =
                                        Glob.defaultOptions
                                 in
                                 { o | include = Glob.OnlyFiles }
                                )
                                "src/**/*.elm"
                                |> BackendTask.andThen
                                    (\files ->
                                        case files of
                                            first :: _ ->
                                                Script.exec "elm" [ "make", first, "--output", "/dev/null" ]

                                            [] ->
                                                BackendTask.succeed ()
                                    )
                in
                fetchCmd
                    |> BackendTask.toResult
                    |> BackendTask.map (\_ -> ())
            )


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
    Set.fromList
        [ "elm/html"
        , "elm/virtual-dom"
        , "elm/browser"
        , "elm/http"
        , "elm/file"

        -- elm/url is NOT skipped: its source files load and parse fine,
        -- and `patchSource` below rewrites `Url.percentEncode` /
        -- `percentDecode` bodies (which would otherwise bottom out at
        -- `Elm.Kernel.Url.*`) to pure-Elm implementations.
        ]


evalErrorKindToString : Types.EvalErrorKind -> String
evalErrorKindToString kind =
    case kind of
        Types.TypeError msg ->
            "type error: " ++ msg

        Types.Unsupported msg ->
            "unsupported: " ++ msg

        Types.NameError name ->
            "could not resolve '" ++ name ++ "'"

        Types.Todo msg ->
            "hit Debug.todo: " ++ msg

        Types.TailCall _ ->
            "internal TCO signal"

        Types.TailCallLocals _ ->
            "internal resolved TCO signal"


patchSource : String -> String
patchSource source =
    source
        |> patchTestRunner
        |> patchUrl


patchTestRunner : String -> String
patchTestRunner source =
    if String.contains "runThunk =\n    Elm.Kernel.Test.runThunk" source then
        source
            |> String.replace
                "runThunk =\n    Elm.Kernel.Test.runThunk"
                "runThunk fn =\n    Ok (fn ())"

    else
        source


{-| Rewrite `elm/url`'s `Url.elm` so `percentEncode` / `percentDecode`
don't reach `Elm.Kernel.Url.*` (which our interpreter has no native
implementation for). The replacement is a self-contained pure-Elm
version inlined into the `Url` module itself, using standard
`Char.toCode` / `String.toList` / UTF-8 bit-twiddling. Only triggers on
the specific body of `elm/url`'s stock `Url.elm`, so if the source ever
changes we'll notice (the patch silently no-ops and tests that need
percent-encoding will fail).
-}
patchUrl : String -> String
patchUrl source =
    if String.contains "Elm.Kernel.Url.percentEncode" source then
        source
            |> String.replace "import Elm.Kernel.Url\n" ""
            |> String.replace
                "percentEncode : String -> String\npercentEncode =\n  Elm.Kernel.Url.percentEncode"
                percentEncodePatched
            |> String.replace
                "percentDecode : String -> Maybe String\npercentDecode =\n  Elm.Kernel.Url.percentDecode"
                percentDecodePatched

    else
        source


percentEncodePatched : String
percentEncodePatched =
    String.join "\n"
        [ "percentEncode : String -> String"
        , "percentEncode str ="
        , "    let"
        , "        hexChar n ="
        , "            if n < 10 then Char.fromCode (0x30 + n)"
        , "            else Char.fromCode (0x41 + n - 10)"
        , "        hexByte n ="
        , "            String.fromList [ '%', hexChar (n // 16), hexChar (modBy 16 n) ]"
        , "        encodeByte code ="
        , "            if (code >= 0x41 && code <= 0x5A)"
        , "                || (code >= 0x61 && code <= 0x7A)"
        , "                || (code >= 0x30 && code <= 0x39)"
        , "                || code == 0x2D || code == 0x5F || code == 0x2E || code == 0x7E"
        , "            then"
        , "                String.fromChar (Char.fromCode code)"
        , "            else"
        , "                hexByte code"
        , "        encodeCodePoint code ="
        , "            if code < 0x80 then"
        , "                encodeByte code"
        , "            else if code < 0x800 then"
        , "                hexByte (0xC0 + (code // 64)) ++ hexByte (0x80 + modBy 64 code)"
        , "            else if code < 0x10000 then"
        , "                hexByte (0xE0 + (code // 4096))"
        , "                    ++ hexByte (0x80 + modBy 64 (code // 64))"
        , "                    ++ hexByte (0x80 + modBy 64 code)"
        , "            else"
        , "                hexByte (0xF0 + (code // 262144))"
        , "                    ++ hexByte (0x80 + modBy 64 (code // 4096))"
        , "                    ++ hexByte (0x80 + modBy 64 (code // 64))"
        , "                    ++ hexByte (0x80 + modBy 64 code)"
        , "    in"
        , "    String.foldr (\\c acc -> encodeCodePoint (Char.toCode c) ++ acc) \"\" str"
        ]


percentDecodePatched : String
percentDecodePatched =
    String.join "\n"
        [ "percentDecode : String -> Maybe String"
        , "percentDecode str ="
        , "    let"
        , "        hexVal c ="
        , "            let code = Char.toCode c"
        , "            in"
        , "            if code >= 0x30 && code <= 0x39 then Just (code - 0x30)"
        , "            else if code >= 0x41 && code <= 0x46 then Just (code - 0x41 + 10)"
        , "            else if code >= 0x61 && code <= 0x66 then Just (code - 0x61 + 10)"
        , "            else Nothing"
        , "        helper remaining acc ="
        , "            case remaining of"
        , "                [] ->"
        , "                    Just (String.fromList (List.reverse acc))"
        , "                '%' :: hi :: lo :: rest ->"
        , "                    case ( hexVal hi, hexVal lo ) of"
        , "                        ( Just h, Just l ) -> helper rest (Char.fromCode (h * 16 + l) :: acc)"
        , "                        _ -> Nothing"
        , "                c :: rest ->"
        , "                    helper rest (c :: acc)"
        , "    in"
        , "    helper (String.toList str) []"
        ]
