module DepGraphTests exposing (suite)

import DepGraph
import Expect
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "DepGraph"
        [ describe "sourcesTestedBy"
            [ test "finds direct source imports from a test file" <|
                \_ ->
                    let
                        graph =
                            DepGraph.buildGraph
                                { sourceDirectories = [ "src", "tests" ]
                                , files =
                                    [ { filePath = "src/MathLib.elm"
                                      , content = "module MathLib exposing (..)\n\nimport Basics\n"
                                      }
                                    , { filePath = "tests/MathLibTest.elm"
                                      , content = "module MathLibTest exposing (..)\n\nimport MathLib\nimport Test\n"
                                      }
                                    ]
                                }
                    in
                    DepGraph.sourcesTestedBy [ "src" ] graph "tests/MathLibTest.elm"
                        |> Expect.equal [ "src/MathLib.elm" ]
            , test "finds transitive source imports" <|
                \_ ->
                    let
                        graph =
                            DepGraph.buildGraph
                                { sourceDirectories = [ "src", "tests" ]
                                , files =
                                    [ { filePath = "src/Utils.elm"
                                      , content = "module Utils exposing (..)\n"
                                      }
                                    , { filePath = "src/MathLib.elm"
                                      , content = "module MathLib exposing (..)\n\nimport Utils\n"
                                      }
                                    , { filePath = "tests/MathLibTest.elm"
                                      , content = "module MathLibTest exposing (..)\n\nimport MathLib\nimport Test\n"
                                      }
                                    ]
                                }
                    in
                    DepGraph.sourcesTestedBy [ "src" ] graph "tests/MathLibTest.elm"
                        |> Expect.equal [ "src/MathLib.elm", "src/Utils.elm" ]
            , test "excludes test files from results" <|
                \_ ->
                    let
                        graph =
                            DepGraph.buildGraph
                                { sourceDirectories = [ "src", "tests" ]
                                , files =
                                    [ { filePath = "src/MathLib.elm"
                                      , content = "module MathLib exposing (..)\n"
                                      }
                                    , { filePath = "tests/TestHelpers.elm"
                                      , content = "module TestHelpers exposing (..)\n"
                                      }
                                    , { filePath = "tests/MathLibTest.elm"
                                      , content = "module MathLibTest exposing (..)\n\nimport MathLib\nimport TestHelpers\nimport Test\n"
                                      }
                                    ]
                                }
                    in
                    DepGraph.sourcesTestedBy [ "src" ] graph "tests/MathLibTest.elm"
                        |> Expect.equal [ "src/MathLib.elm" ]
            , test "excludes the test file itself" <|
                \_ ->
                    let
                        graph =
                            DepGraph.buildGraph
                                { sourceDirectories = [ "src", "tests" ]
                                , files =
                                    [ { filePath = "src/MathLib.elm"
                                      , content = "module MathLib exposing (..)\n"
                                      }
                                    , { filePath = "tests/MathLibTest.elm"
                                      , content = "module MathLibTest exposing (..)\n\nimport MathLib\n"
                                      }
                                    ]
                                }
                    in
                    DepGraph.sourcesTestedBy [ "src" ] graph "tests/MathLibTest.elm"
                        |> Expect.equal [ "src/MathLib.elm" ]
            , test "returns empty list when test has no source imports" <|
                \_ ->
                    let
                        graph =
                            DepGraph.buildGraph
                                { sourceDirectories = [ "src", "tests" ]
                                , files =
                                    [ { filePath = "src/MathLib.elm"
                                      , content = "module MathLib exposing (..)\n"
                                      }
                                    , { filePath = "tests/StandaloneTest.elm"
                                      , content = "module StandaloneTest exposing (..)\n\nimport Test\n"
                                      }
                                    ]
                                }
                    in
                    DepGraph.sourcesTestedBy [ "src" ] graph "tests/StandaloneTest.elm"
                        |> Expect.equal []
            , test "works with multiple source directories" <|
                \_ ->
                    let
                        graph =
                            DepGraph.buildGraph
                                { sourceDirectories = [ "src", "gen", "tests" ]
                                , files =
                                    [ { filePath = "src/MathLib.elm"
                                      , content = "module MathLib exposing (..)\n"
                                      }
                                    , { filePath = "gen/Generated.elm"
                                      , content = "module Generated exposing (..)\n"
                                      }
                                    , { filePath = "tests/MyTest.elm"
                                      , content = "module MyTest exposing (..)\n\nimport MathLib\nimport Generated\n"
                                      }
                                    ]
                                }
                    in
                    DepGraph.sourcesTestedBy [ "src", "gen" ] graph "tests/MyTest.elm"
                        |> List.sort
                        |> Expect.equal [ "gen/Generated.elm", "src/MathLib.elm" ]
            ]
        ]
