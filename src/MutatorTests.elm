module MutatorTests exposing (suite)

import Expect
import Mutator exposing (Mutation)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Mutator"
        [ describe "negateCondition"
            [ test "wraps simple condition with not" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    if x then\n        \"yes\"\n    else\n        \"no\""

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "negateCondition")
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "not x"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 negateCondition, got " ++ String.fromInt (List.length mutations))
            , test "wraps complex condition with parens" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    if x > 0 then\n        1\n    else\n        0"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "negateCondition")
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "not (x > 0)"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 negateCondition, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "swapComparison"
            [ test "swaps > to >=" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    x > 0"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "swapComparison" && String.contains ">=" m.description)
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "x >= 0"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 swapComparison, got " ++ String.fromInt (List.length mutations))
            , test "swaps == to /=" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    x == 0"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "swapComparison" && String.contains "/=" m.description)
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "x /= 0"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 swapComparison, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "swapBooleanLiteral"
            [ test "swaps True to False" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    True"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "swapBooleanLiteral")
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "False"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 swapBooleanLiteral, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "replaceArithmetic"
            [ test "swaps + to -" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo a b =\n    a + b"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "replaceArithmetic")
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "a - b"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 replaceArithmetic, got " ++ String.fromInt (List.length mutations))
            ]

        -- NEW OPERATORS - tests first (red), then implement (green)
        , describe "swapLogicalOperators"
            [ test "swaps && to ||" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo a b =\n    a && b"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "swapLogicalOperator")
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "a || b"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 swapLogicalOperator, got " ++ String.fromInt (List.length mutations))
            , test "swaps || to &&" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo a b =\n    a || b"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "swapLogicalOperator")
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "a && b"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 swapLogicalOperator, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "replaceIntLiteral"
            [ test "replaces 0 with 1" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    0"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "replaceIntLiteral")
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "\n    1"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 replaceIntLiteral, got " ++ String.fromInt (List.length mutations))
            , test "replaces 1 with 0" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    1"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "replaceIntLiteral")
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "\n    0"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 replaceIntLiteral, got " ++ String.fromInt (List.length mutations))
            , test "replaces other int with 0" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    42"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "replaceIntLiteral")
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "\n    0"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 replaceIntLiteral, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "replaceStringLiteral"
            [ test "replaces non-empty string with empty" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    \"hello\""

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "replaceStringLiteral")
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "\n    \"\""
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 replaceStringLiteral, got " ++ String.fromInt (List.length mutations))
            , test "replaces empty string with a value" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    \"\""

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "replaceStringLiteral")
                    in
                    case mutations of
                        [ m ] ->
                            (m.mutatedSource |> String.contains "\"\"")
                                |> Expect.equal False

                        _ ->
                            Expect.fail ("Expected 1 replaceStringLiteral, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "parse failure"
            [ test "returns empty list for invalid source" <|
                \_ ->
                    Mutator.generateMutations "not valid"
                        |> Expect.equal []
            ]
        ]
