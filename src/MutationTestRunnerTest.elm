module MutationTestRunnerTest exposing (suite)

{-| Integration tests for the mutation test runner using Test.BackendTask.

These tests verify the BackendTask-based flows (file discovery, source loading)
against a virtual filesystem — no real files or network needed.

-}

import BackendTask
import BackendTask.File as File
import BackendTask.Glob as Glob
import DepGraph
import Dict
import Expect
import FatalError exposing (FatalError)
import Json.Decode
import Pages.Script as Script
import Test exposing (Test, describe, test)
import Test.BackendTask as BackendTaskTest


suite : Test
suite =
    describe "MutationTestRunner integration"
        [ describe "test file auto-discovery"
            [ test "finds test file that imports the mutated module" <|
                \() ->
                    findTestFilesImporting "MyModule"
                        |> BackendTaskTest.fromBackendTaskWith
                            (BackendTaskTest.init
                                |> BackendTaskTest.withFile "tests/MyModuleTest.elm"
                                    "module MyModuleTest exposing (suite)\n\nimport MyModule\nimport Test exposing (Test)\n\nsuite : Test\nsuite = Test.describe \"MyModule\" []\n"
                                |> BackendTaskTest.withFile "tests/OtherTest.elm"
                                    "module OtherTest exposing (suite)\n\nimport OtherModule\nimport Test exposing (Test)\n\nsuite : Test\nsuite = Test.describe \"Other\" []\n"
                            )
                        |> BackendTaskTest.ensureStdout [ "tests/MyModuleTest.elm" ]
                        |> BackendTaskTest.expectSuccess
            , test "finds multiple test files that import the module" <|
                \() ->
                    findTestFilesImporting "Shared"
                        |> BackendTaskTest.fromBackendTaskWith
                            (BackendTaskTest.init
                                |> BackendTaskTest.withFile "tests/UnitTest.elm"
                                    "module UnitTest exposing (suite)\n\nimport Shared\nimport Test exposing (Test)\n\nsuite : Test\nsuite = Test.describe \"Unit\" []\n"
                                |> BackendTaskTest.withFile "tests/IntegrationTest.elm"
                                    "module IntegrationTest exposing (suite)\n\nimport Shared\nimport Test exposing (Test)\n\nsuite : Test\nsuite = Test.describe \"Integration\" []\n"
                            )
                        |> BackendTaskTest.ensureStdout [ "tests/IntegrationTest.elm,tests/UnitTest.elm" ]
                        |> BackendTaskTest.expectSuccess
            , test "returns empty when no test imports the module" <|
                \() ->
                    findTestFilesImporting "Orphan"
                        |> BackendTaskTest.fromBackendTaskWith
                            (BackendTaskTest.init
                                |> BackendTaskTest.withFile "tests/SomeTest.elm"
                                    "module SomeTest exposing (suite)\n\nimport Other\nimport Test exposing (Test)\n\nsuite : Test\nsuite = Test.describe \"Some\" []\n"
                            )
                        |> BackendTaskTest.ensureStdout [ "" ]
                        |> BackendTaskTest.expectSuccess
            ]
        , describe "elm.json parsing"
            [ test "reads source-directories from application elm.json" <|
                \() ->
                    readSourceDirs
                        |> BackendTaskTest.fromBackendTaskWith
                            (BackendTaskTest.init
                                |> BackendTaskTest.withFile "elm.json"
                                    """{"type": "application", "source-directories": ["src", "gen"], "elm-version": "0.19.1", "dependencies": {"direct": {}, "indirect": {}}, "test-dependencies": {"direct": {}, "indirect": {}}}"""
                            )
                        |> BackendTaskTest.ensureStdout [ "src,gen" ]
                        |> BackendTaskTest.expectSuccess
            , test "defaults to src for package elm.json" <|
                \() ->
                    readSourceDirs
                        |> BackendTaskTest.fromBackendTaskWith
                            (BackendTaskTest.init
                                |> BackendTaskTest.withFile "elm.json"
                                    """{"type": "package", "name": "author/pkg", "version": "1.0.0", "summary": "test", "license": "BSD-3-Clause", "elm-version": "0.19.0 <= v < 0.20.0", "exposed-modules": [], "dependencies": {}, "test-dependencies": {}}"""
                            )
                        |> BackendTaskTest.ensureStdout [ "src" ]
                        |> BackendTaskTest.expectSuccess
            , test "includes test-dependencies from application elm.json" <|
                \() ->
                    readDeps
                        |> BackendTaskTest.fromBackendTaskWith
                            (BackendTaskTest.init
                                |> BackendTaskTest.withFile "elm.json"
                                    """{"type": "application", "source-directories": ["src"], "elm-version": "0.19.1", "dependencies": {"direct": {"elm/core": "1.0.5"}, "indirect": {}}, "test-dependencies": {"direct": {"elm-explorations/test": "2.2.1"}, "indirect": {"elm/random": "1.0.0"}}}"""
                            )
                        |> BackendTaskTest.ensureStdout [ "elm-explorations/test,elm/random" ]
                        |> BackendTaskTest.expectSuccess
            , test "includes test-dependencies from package elm.json" <|
                \() ->
                    readDeps
                        |> BackendTaskTest.fromBackendTaskWith
                            (BackendTaskTest.init
                                |> BackendTaskTest.withFile "elm.json"
                                    """{"type": "package", "name": "author/pkg", "version": "1.0.0", "summary": "test", "license": "BSD-3-Clause", "elm-version": "0.19.0 <= v < 0.20.0", "exposed-modules": [], "dependencies": {"elm/core": "1.0.0 <= v < 2.0.0"}, "test-dependencies": {"elm-explorations/test": "2.0.0 <= v < 3.0.0"}}"""
                            )
                        |> BackendTaskTest.ensureStdout [ "elm-explorations/test" ]
                        |> BackendTaskTest.expectSuccess
            ]
        ]



-- Test helpers that wrap BackendTask flows and output results via Script.log


findTestFilesImporting : String -> BackendTask.BackendTask FatalError ()
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
                                |> List.sort
                                |> String.join ","
                        )
            )
        |> BackendTask.andThen (\result -> Script.log result)


readSourceDirs : BackendTask.BackendTask FatalError ()
readSourceDirs =
    File.rawFile "elm.json"
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\raw ->
                let
                    result =
                        case Json.Decode.decodeString sourceDirsDecoder raw of
                            Ok dirs ->
                                String.join "," dirs

                            Err _ ->
                                "ERROR"
                in
                Script.log result
            )


readDeps : BackendTask.BackendTask FatalError ()
readDeps =
    File.rawFile "elm.json"
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\raw ->
                let
                    result =
                        case Json.Decode.decodeString depsDecoder raw of
                            Ok deps ->
                                deps
                                    |> List.filter (\name -> name /= "elm/core")
                                    |> String.join ","

                            Err _ ->
                                "ERROR"
                in
                Script.log result
            )


sourceDirsDecoder : Json.Decode.Decoder (List String)
sourceDirsDecoder =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\elmJsonType ->
                if elmJsonType == "package" then
                    Json.Decode.succeed [ "src" ]

                else
                    Json.Decode.field "source-directories" (Json.Decode.list Json.Decode.string)
            )


depsDecoder : Json.Decode.Decoder (List String)
depsDecoder =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\elmJsonType ->
                if elmJsonType == "package" then
                    Json.Decode.map2 (++)
                        (Json.Decode.field "dependencies" (Json.Decode.dict Json.Decode.string) |> Json.Decode.map Dict.keys)
                        (Json.Decode.oneOf
                            [ Json.Decode.field "test-dependencies" (Json.Decode.dict Json.Decode.string) |> Json.Decode.map Dict.keys
                            , Json.Decode.succeed []
                            ]
                        )

                else
                    Json.Decode.map4 (\a b c d -> a ++ b ++ c ++ d)
                        (Json.Decode.at [ "dependencies", "direct" ] (Json.Decode.dict Json.Decode.string) |> Json.Decode.map Dict.keys)
                        (Json.Decode.at [ "dependencies", "indirect" ] (Json.Decode.dict Json.Decode.string) |> Json.Decode.map Dict.keys)
                        (Json.Decode.oneOf
                            [ Json.Decode.at [ "test-dependencies", "direct" ] (Json.Decode.dict Json.Decode.string) |> Json.Decode.map Dict.keys
                            , Json.Decode.succeed []
                            ]
                        )
                        (Json.Decode.oneOf
                            [ Json.Decode.at [ "test-dependencies", "indirect" ] (Json.Decode.dict Json.Decode.string) |> Json.Decode.map Dict.keys
                            , Json.Decode.succeed []
                            ]
                        )
            )
