module TestAnalysisTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import TestAnalysis


suite : Test
suite =
    describe "TestAnalysis"
        [ discoverTestValuesViaAnnotation
        ]


{-| `MutationTestRunner` uses `discoverTestValues` as a fast-path
alternative to interpreter probing. The interpreter probe used to run
every test via `runToString`, which blew the JS call stack on suites
like elm-ical's `ExpansionTests` (~2700 lines). The runner now prefers
the static type-annotation scan, so these cases need to keep working.
-}
discoverTestValuesViaAnnotation : Test
discoverTestValuesViaAnnotation =
    describe "discoverTestValues finds suites via static type annotation"
        [ test "exposed value annotated `: Test` is found" <|
            \_ ->
                elmSource
                    [ "module MyTests exposing (suite)"
                    , ""
                    , "import Test exposing (Test)"
                    , ""
                    , ""
                    , "suite : Test"
                    , "suite = Test.todo \"\""
                    ]
                    |> TestAnalysis.discoverTestValues
                    |> Expect.equal [ "suite" ]
        , test "exposed value annotated `: Test.Test` is found" <|
            \_ ->
                elmSource
                    [ "module MyTests exposing (suite)"
                    , ""
                    , "import Test"
                    , ""
                    , ""
                    , "suite : Test.Test"
                    , "suite = Test.todo \"\""
                    ]
                    |> TestAnalysis.discoverTestValues
                    |> Expect.equal [ "suite" ]
        , test "non-exposed annotated values are skipped" <|
            \_ ->
                elmSource
                    [ "module MyTests exposing (suite)"
                    , ""
                    , "import Test exposing (Test)"
                    , ""
                    , ""
                    , "suite : Test"
                    , "suite = helper"
                    , ""
                    , ""
                    , "helper : Test"
                    , "helper = Test.todo \"\""
                    ]
                    |> TestAnalysis.discoverTestValues
                    |> Expect.equal [ "suite" ]
        , test "values without an annotation are not discovered statically" <|
            \_ ->
                elmSource
                    [ "module MyTests exposing (suite)"
                    , ""
                    , "import Test"
                    , ""
                    , ""
                    , "suite ="
                    , "    Test.todo \"\""
                    ]
                    |> TestAnalysis.discoverTestValues
                    |> Expect.equal []
        , test "multiple annotated Test values are all found" <|
            \_ ->
                elmSource
                    [ "module MyTests exposing (suiteA, suiteB)"
                    , ""
                    , "import Test exposing (Test)"
                    , ""
                    , ""
                    , "suiteA : Test"
                    , "suiteA = Test.todo \"a\""
                    , ""
                    , ""
                    , "suiteB : Test"
                    , "suiteB = Test.todo \"b\""
                    ]
                    |> TestAnalysis.discoverTestValues
                    |> Expect.equal [ "suiteA", "suiteB" ]
        ]


elmSource : List String -> String
elmSource lines =
    String.join "\n" (lines ++ [ "" ])
