module RunCoreExtraTests exposing (run)

{-| Run elmcraft/core-extra's test suite via the cached interpreter.

    time bunx elm-pages run src/RunCoreExtraTests.elm -- --build .build/core-extra

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
import FatalError exposing (FatalError)
import InterpreterProject
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import Set


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


{-| The expression to evaluate in the wrapper module.
-}
buildExpression : String
buildExpression =
    "SimpleTestRunner.runToString (Test.describe \"core-extra\" [ "
        ++ (testModuleSuites |> String.join ", ")
        ++ " ])"


{-| The imports needed by the wrapper expression.
-}
wrapperImports : List String
wrapperImports =
    testModuleImports ++ [ "SimpleTestRunner", "Test" ]


task : Config -> BackendTask FatalError ()
task config =
    Do.do
        (BackendTask.Extra.timed "[profile] InterpreterProject.loadWith" "[profile] InterpreterProject.loadWith done"
            (InterpreterProject.loadWith
                { projectDir = Path.path "."
                , skipPackages = skipPackages
                , patchSource = patchSource
                , extraSourceFiles = [ "src/SimpleTestRunner.elm" ]
                , sourceDirectories = Just [ coreExtraDir ++ "/tests" ]
                }
            )
        )
    <| \project ->
    Do.exec "mkdir" [ "-p", Path.toString config.buildDirectory ] <| \_ ->
    Do.do
        (BackendTask.Extra.timed "[profile] Cache.run (evalWith)" "[profile] Cache.run (evalWith) done"
            (Cache.run { jobs = Nothing } config.buildDirectory
                (InterpreterProject.evalWith project
                    { imports = wrapperImports
                    , expression = buildExpression
                    }
                    Cache.succeed
                )
            )
        )
    <| \result ->
    Do.allowFatal (File.rawFile (Path.toString result.output)) <| \output ->
    Do.do (displayResults output) <| \_ ->
    Do.noop


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


displayResults : String -> BackendTask FatalError ()
displayResults output =
    let
        lines : List String
        lines =
            String.lines output

        summaryParts : { passed : Int, failed : Int, total : Int }
        summaryParts =
            case String.split "," (List.head lines |> Maybe.withDefault "") of
                [ p, f, t ] ->
                    { passed = String.toInt p |> Maybe.withDefault 0
                    , failed = String.toInt f |> Maybe.withDefault 0
                    , total = String.toInt t |> Maybe.withDefault 0
                    }

                _ ->
                    { passed = 0, failed = 0, total = 0 }

        failLines : List String
        failLines =
            List.drop 1 lines
                |> List.filter (String.startsWith "FAIL:")
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
