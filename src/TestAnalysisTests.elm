module TestAnalysisTests exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import TestAnalysis


suite : Test
suite =
    describe "TestAnalysis"
        [ describe "getCandidateNames"
            [ test "finds all zero-arg exposed values" <|
                \_ ->
                    TestAnalysis.getCandidateNames
                        (String.join "\n"
                            [ "module M exposing (..)"
                            , ""
                            , "foo = 1"
                            , "bar = 2"
                            , "baz x = x"
                            ]
                        )
                        |> Expect.equal [ "foo", "bar" ]
            , test "respects explicit exposing list" <|
                \_ ->
                    TestAnalysis.getCandidateNames
                        (String.join "\n"
                            [ "module M exposing (foo)"
                            , ""
                            , "foo = 1"
                            , "bar = 2"
                            ]
                        )
                        |> Expect.equal [ "foo" ]
            , test "excludes functions with arguments" <|
                \_ ->
                    TestAnalysis.getCandidateNames
                        (String.join "\n"
                            [ "module M exposing (..)"
                            , ""
                            , "value = 1"
                            , "func x = x"
                            , "func2 a b = a + b"
                            ]
                        )
                        |> Expect.equal [ "value" ]
            , test "returns empty for invalid source" <|
                \_ ->
                    TestAnalysis.getCandidateNames "not valid"
                        |> Expect.equal []
            ]
        , describe "discoverTestValues (heuristic)"
            [ test "finds suite : Test" <|
                \_ ->
                    TestAnalysis.discoverTestValues
                        (String.join "\n"
                            [ "module MyTests exposing (suite)"
                            , ""
                            , "import Test exposing (Test)"
                            , ""
                            , "suite : Test"
                            , "suite = Test.describe \"foo\" []"
                            ]
                        )
                        |> Expect.equal [ "suite" ]
            , test "finds multiple Test values" <|
                \_ ->
                    TestAnalysis.discoverTestValues
                        (String.join "\n"
                            [ "module MyTests exposing (unitTests, integrationTests)"
                            , ""
                            , "import Test exposing (Test)"
                            , ""
                            , "unitTests : Test"
                            , "unitTests = Test.describe \"unit\" []"
                            , ""
                            , "integrationTests : Test"
                            , "integrationTests = Test.describe \"integration\" []"
                            ]
                        )
                        |> Expect.equal [ "unitTests", "integrationTests" ]
            , test "finds Test.Test qualified annotation" <|
                \_ ->
                    TestAnalysis.discoverTestValues
                        (String.join "\n"
                            [ "module MyTests exposing (suite)"
                            , ""
                            , "import Test"
                            , ""
                            , "suite : Test.Test"
                            , "suite = Test.describe \"foo\" []"
                            ]
                        )
                        |> Expect.equal [ "suite" ]
            , test "ignores non-Test values" <|
                \_ ->
                    TestAnalysis.discoverTestValues
                        (String.join "\n"
                            [ "module MyTests exposing (suite, helper)"
                            , ""
                            , "import Test exposing (Test)"
                            , ""
                            , "suite : Test"
                            , "suite = Test.describe \"foo\" []"
                            , ""
                            , "helper : String"
                            , "helper = \"hi\""
                            ]
                        )
                        |> Expect.equal [ "suite" ]
            , test "only returns exposed values" <|
                \_ ->
                    TestAnalysis.discoverTestValues
                        (String.join "\n"
                            [ "module MyTests exposing (suite)"
                            , ""
                            , "import Test exposing (Test)"
                            , ""
                            , "suite : Test"
                            , "suite = Test.describe \"foo\" [internal]"
                            , ""
                            , "internal : Test"
                            , "internal = Test.describe \"internal\" []"
                            ]
                        )
                        |> Expect.equal [ "suite" ]
            , test "handles exposing all" <|
                \_ ->
                    TestAnalysis.discoverTestValues
                        (String.join "\n"
                            [ "module MyTests exposing (..)"
                            , ""
                            , "import Test exposing (Test)"
                            , ""
                            , "suite : Test"
                            , "suite = Test.describe \"foo\" []"
                            , ""
                            , "other : Test"
                            , "other = Test.describe \"other\" []"
                            ]
                        )
                        |> Expect.equal [ "suite", "other" ]
            , test "returns empty for unparseable source" <|
                \_ ->
                    TestAnalysis.discoverTestValues "not valid elm"
                        |> Expect.equal []
            ]
        , describe "extractDescribeChildren"
            [ test "extracts children from describe with list literal" <|
                \_ ->
                    let
                        source =
                            String.join "\n"
                                [ "module T exposing (suite)"
                                , "import Test exposing (Test, describe, test)"
                                , "import Expect"
                                , ""
                                , "suite : Test"
                                , "suite ="
                                , "    describe \"tests\""
                                , "        [ test \"a\" (\\_ -> Expect.equal 1 1)"
                                , "        , test \"b\" (\\_ -> Expect.equal 2 2)"
                                , "        ]"
                                ]
                    in
                    case TestAnalysis.extractDescribeChildren "suite" source of
                        Just children ->
                            List.length children
                                |> Expect.equal 2

                        Nothing ->
                            Expect.fail "Expected to extract 2 children"
            , test "first child source starts with test" <|
                \_ ->
                    let
                        source =
                            String.join "\n"
                                [ "module T exposing (suite)"
                                , "import Test exposing (Test, describe, test)"
                                , "import Expect"
                                , ""
                                , "suite ="
                                , "    describe \"tests\""
                                , "        [ test \"a\" (\\_ -> Expect.equal 1 1)"
                                , "        , test \"b\" (\\_ -> Expect.equal 2 2)"
                                , "        ]"
                                ]
                    in
                    case TestAnalysis.extractDescribeChildren "suite" source of
                        Just (first :: _) ->
                            String.startsWith "test" first
                                |> Expect.equal True

                        _ ->
                            Expect.fail "Expected at least 1 child"
            ]
        ]
