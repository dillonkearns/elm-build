module TestAnalysisTest exposing (suite)

import Elm.Parser
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.Node
import Expect
import Test exposing (Test, describe, test)
import TestAnalysis


suite : Test
suite =
    describe "TestAnalysis"
        [ discoverTestValuesViaAnnotation
        , staticRunnerCounts
        ]


{-| `MutationTestRunner` used to skip per-child precision whenever the
children of `describe` were inline expressions (like `test "foo" <| \() ->
...`), because the runner counted per-child runners by calling
`SimpleTestRunner.countTests Mod.childName` through the interpreter.
For inline children there's no `childName` to pass.

`countRunnersStatically` lets the runner compute the same counts by
walking the AST. Lock in the common patterns so the fast path keeps
firing as the mutator / tests evolve.
-}
staticRunnerCounts : Test
staticRunnerCounts =
    describe "countRunnersStatically handles common inline test shapes"
        [ test "single `test label fn` counts as 1 runner" <|
            \_ ->
                parseExpr "test \"a\" (\\() -> Expect.pass)"
                    |> Maybe.andThen TestAnalysis.countRunnersStatically
                    |> Expect.equal (Just 1)
        , test "single `test label <| fn` counts as 1 runner" <|
            \_ ->
                parseExpr "test \"a\" <| \\() -> Expect.pass"
                    |> Maybe.andThen TestAnalysis.countRunnersStatically
                    |> Expect.equal (Just 1)
        , test "`fuzz fuzzer label <| fn` counts as 1 runner" <|
            \_ ->
                parseExpr "fuzz int \"a\" <| \\n -> Expect.pass"
                    |> Maybe.andThen TestAnalysis.countRunnersStatically
                    |> Expect.equal (Just 1)
        , test "`fuzz2` counts as 1 runner" <|
            \_ ->
                parseExpr "fuzz2 int int \"a\" <| \\a b -> Expect.pass"
                    |> Maybe.andThen TestAnalysis.countRunnersStatically
                    |> Expect.equal (Just 1)
        , test "`describe label [test, test, test]` sums its children" <|
            \_ ->
                parseExpr
                    (String.join "\n"
                        [ "describe \"group\""
                        , "    [ test \"a\" <| \\() -> Expect.pass"
                        , "    , test \"b\" <| \\() -> Expect.pass"
                        , "    , test \"c\" <| \\() -> Expect.pass"
                        , "    ]"
                        ]
                    )
                    |> Maybe.andThen TestAnalysis.countRunnersStatically
                    |> Expect.equal (Just 3)
        , test "`describe \"outer\" <| [describe \"inner\" [2 tests], test]` pipes through `<|`" <|
            \_ ->
                parseExpr
                    (String.join "\n"
                        [ "describe \"outer\" <|"
                        , "    [ describe \"inner\""
                        , "        [ test \"x\" <| \\() -> Expect.pass"
                        , "        , test \"y\" <| \\() -> Expect.pass"
                        , "        ]"
                        , "    , test \"z\" <| \\() -> Expect.pass"
                        , "    ]"
                        ]
                    )
                    |> Maybe.andThen TestAnalysis.countRunnersStatically
                    |> Expect.equal (Just 3)
        , test "nested `describe` recurses correctly" <|
            \_ ->
                parseExpr
                    (String.join "\n"
                        [ "describe \"outer\""
                        , "    [ describe \"inner\""
                        , "        [ test \"a\" <| \\() -> Expect.pass"
                        , "        , test \"b\" <| \\() -> Expect.pass"
                        , "        ]"
                        , "    , test \"c\" <| \\() -> Expect.pass"
                        , "    ]"
                        ]
                    )
                    |> Maybe.andThen TestAnalysis.countRunnersStatically
                    |> Expect.equal (Just 3)
        , test "parenthesized expressions unwrap transparently" <|
            \_ ->
                parseExpr "(test \"a\" <| \\() -> Expect.pass)"
                    |> Maybe.andThen TestAnalysis.countRunnersStatically
                    |> Expect.equal (Just 1)
        , test "Test.describe (qualified) works the same" <|
            \_ ->
                parseExpr
                    (String.join "\n"
                        [ "Test.describe \"group\""
                        , "    [ Test.test \"a\" <| \\() -> Expect.pass"
                        , "    , Test.test \"b\" <| \\() -> Expect.pass"
                        , "    ]"
                        ]
                    )
                    |> Maybe.andThen TestAnalysis.countRunnersStatically
                    |> Expect.equal (Just 2)
        , test "unknown call (helper function returning Test) gives up" <|
            \_ ->
                parseExpr
                    (String.join "\n"
                        [ "describe \"group\""
                        , "    [ test \"a\" <| \\() -> Expect.pass"
                        , "    , myHelper 42"
                        , "    ]"
                        ]
                    )
                    |> Maybe.andThen TestAnalysis.countRunnersStatically
                    |> Expect.equal Nothing
        , test "bare reference to a named child gives up (not statically countable)" <|
            \_ ->
                parseExpr "describe \"group\" [ namedTest ]"
                    |> Maybe.andThen TestAnalysis.countRunnersStatically
                    |> Expect.equal Nothing
        ]


parseExpr : String -> Maybe (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
parseExpr exprText =
    let
        source : String
        source =
            String.join "\n"
                [ "module Probe exposing (probe)"
                , ""
                , "import Expect"
                , "import Fuzz exposing (int)"
                , "import Test exposing (..)"
                , ""
                , ""
                , "probe ="
                , exprText
                    |> String.lines
                    |> List.map (\line -> "    " ++ line)
                    |> String.join "\n"
                , ""
                ]
    in
    case Elm.Parser.parseToFile source of
        Err _ ->
            Nothing

        Ok file ->
            file.declarations
                |> List.filterMap
                    (\(Elm.Syntax.Node.Node _ decl) ->
                        case decl of
                            Elm.Syntax.Declaration.FunctionDeclaration func ->
                                Just (Elm.Syntax.Node.value func.declaration |> .expression)

                            _ ->
                                Nothing
                    )
                |> List.head


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
