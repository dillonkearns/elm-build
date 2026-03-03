module RunCoreExtraTests exposing (run)

{-| Run elmcraft/core-extra's test suite via the cached interpreter.

    NODE_OPTIONS="--max-old-space-size=8192 --expose-gc" time bunx elm-pages run src/RunCoreExtraTests.elm -- --build .build/core-extra

-}

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import Cache
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import FatalError exposing (FatalError)
import Eval.Module
import InterpreterProject exposing (InterpreterProject)
import Json.Decode
import Json.Encode
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


type alias TestModule =
    { imports : List String
    , suiteExpression : String
    }


{-| Each test module evaluated independently to avoid OOM.
-}
testModules : List TestModule
testModules =
    [ { imports = [ "ArrayTests" ], suiteExpression = "ArrayTests.suite" }
    , { imports = [ "BasicsTests" ], suiteExpression = "BasicsTests.suite" }
    , { imports = [ "CharTests" ], suiteExpression = "CharTests.suite" }
    , { imports = [ "DictTests" ], suiteExpression = "DictTests.suite" }
    , { imports = [ "FloatTests", "Utils" ]
      , suiteExpression = "Test.describe \"Float.Extra\" [FloatTests.modByTests, FloatTests.testAboutEqual, FloatTests.testBoundaryValuesAsUnicode, FloatTests.testEqualWithin, FloatTests.testInterpolateFrom, FloatTests.testRange, FloatTests.testToFixedDecimalPlaces, FloatTests.testToFixedSignificantDigits]"
      }
    , { imports = [ "ListTests" ], suiteExpression = "ListTests.all" }
    , { imports = [ "MaybeTests" ], suiteExpression = "MaybeTests.suite" }
    , { imports = [ "OrderTests" ], suiteExpression = "OrderTests.all" }
    , { imports = [ "SetTests" ], suiteExpression = "SetTests.all" }
    , { imports = [ "String.NonEmptyTest" ], suiteExpression = "String.NonEmptyTest.nonEmptyTest" }
    -- String.RemoveAccentsTest and String.RemoveDiacriticsTests skipped:
    -- they use removeDiacritics which creates a 65K-element Array via
    -- Array.initialize, too expensive for the interpreter.
    , { imports = [ "String.ReplaceSliceTest" ], suiteExpression = "String.ReplaceSliceTest.replaceSliceTest" }
    , { imports = [ "String.UnicodeTests" ], suiteExpression = "String.UnicodeTests.unicodeTests" }
    , { imports = [ "String.UnindentTest" ], suiteExpression = "String.UnindentTest.unindentTest" }
    , { imports = [ "TripleTests" ], suiteExpression = "TripleTests.suite" }
    ]


{-| Build the expression and imports for a single test module.
-}
buildModuleEval : TestModule -> { imports : List String, expression : String }
buildModuleEval mod =
    { imports = mod.imports ++ [ "SimpleTestRunner", "Test" ]
    , expression = "SimpleTestRunner.runToString (" ++ mod.suiteExpression ++ ")"
    }


task : Config -> BackendTask FatalError ()
task config =
    Do.do
        (BackendTask.Extra.timed "[profile] InterpreterProject.loadWith" "[profile] InterpreterProject.loadWith done"
            (InterpreterProject.loadWith
                { projectDir = Path.path "."
                , skipPackages = skipPackages
                , patchSource = patchSource
                , extraSourceFiles = [ "src/SimpleTestRunner.elm" ]
                , sourceDirectories = Just [ coreExtraDir ++ "/src", coreExtraDir ++ "/tests" ]
                }
            )
        )
    <| \project ->
    let
        packageEnv : Eval.Module.ProjectEnv
        packageEnv =
            InterpreterProject.getPackageEnvFor project
                (List.map buildModuleEval testModules)
    in
    Do.exec "mkdir" [ "-p", Path.toString config.buildDirectory ] <| \_ ->
    evalModulesWithGC packageEnv project config testModules { passed = 0, failed = 0, total = 0, failLines = [] }
    <| \combined ->
    Do.each combined.failLines
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
                    ++ String.fromInt combined.passed
                    ++ ", \"failed\": "
                    ++ String.fromInt combined.failed
                    ++ ", \"total\": "
                    ++ String.fromInt combined.total
                    ++ " }"

            color =
                if combined.failed == 0 then
                    Ansi.Color.fontColor Ansi.Color.brightGreen

                else
                    Ansi.Color.fontColor Ansi.Color.brightRed
         in
         color summary
        )
    <| \_ ->
    Do.noop


type alias ModuleResult =
    { passed : Int
    , failed : Int
    , total : Int
    , failLines : List String
    }


{-| Process modules one at a time with forced GC between evaluations to prevent OOM.
-}
evalModulesWithGC : Eval.Module.ProjectEnv -> InterpreterProject -> Config -> List TestModule -> ModuleResult -> (ModuleResult -> BackendTask FatalError a) -> BackendTask FatalError a
evalModulesWithGC packageEnv project config modules acc continuation =
    case modules of
        [] ->
            continuation acc

        mod :: rest ->
            Do.do (evalModule packageEnv project config mod) <| \result ->
            let
                newAcc =
                    { passed = acc.passed + result.passed
                    , failed = acc.failed + result.failed
                    , total = acc.total + result.total
                    , failLines = acc.failLines ++ result.failLines
                    }
            in
            Do.do forceGC <| \_ ->
            evalModulesWithGC packageEnv project config rest newAcc continuation


{-| Request V8 garbage collection. Requires node --expose-gc flag.
-}
forceGC : BackendTask FatalError ()
forceGC =
    BackendTask.Custom.run "forceGC" Json.Encode.null (Json.Decode.succeed ())
        |> BackendTask.allowFatal


evalModule : Eval.Module.ProjectEnv -> InterpreterProject -> Config -> TestModule -> BackendTask FatalError ModuleResult
evalModule packageEnv project config mod =
    let
        evalConfig =
            buildModuleEval mod
    in
    Do.do
        (Cache.run { jobs = Nothing } config.buildDirectory
            (InterpreterProject.evalWithPackageEnv packageEnv project evalConfig Cache.succeed)
        )
    <| \result ->
    Do.allowFatal (File.rawFile (Path.toString result.output)) <| \output ->
    BackendTask.succeed (parseOutput output)


parseOutput : String -> ModuleResult
parseOutput output =
    let
        lines =
            String.lines output

        summaryParts =
            case String.split "," (List.head lines |> Maybe.withDefault "") of
                [ p, f, t ] ->
                    { passed = String.toInt p |> Maybe.withDefault 0
                    , failed = String.toInt f |> Maybe.withDefault 0
                    , total = String.toInt t |> Maybe.withDefault 0
                    }

                _ ->
                    { passed = 0, failed = 0, total = 0 }

        failLines =
            List.drop 1 lines
                |> List.filter (String.startsWith "FAIL:")
    in
    { passed = summaryParts.passed
    , failed = summaryParts.failed
    , total = summaryParts.total
    , failLines = failLines
    }


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
