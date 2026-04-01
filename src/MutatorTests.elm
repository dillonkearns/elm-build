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
        , describe "removeListElement"
            [ test "removes an element from a 3-element list" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    [ 1, 2, 3 ]"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removeListElement")
                    in
                    -- Should generate 3 mutations: remove each element
                    List.length mutations
                        |> Expect.equal 3
            , test "first removal drops the first element" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    [ 1, 2, 3 ]"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removeListElement")
                    in
                    case mutations of
                        first :: _ ->
                            first.mutatedSource
                                |> String.contains "[ 2, 3 ]"
                                |> Expect.equal True

                        [] ->
                            Expect.fail "Expected at least 1 removeListElement"
            , test "does not generate for single-element list" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    [ 1 ]"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removeListElement")
                    in
                    List.length mutations
                        |> Expect.equal 0
            , test "does not generate for empty list" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    []"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removeListElement")
                    in
                    List.length mutations
                        |> Expect.equal 0
            ]
        , describe "replaceWithNothing"
            [ test "replaces Just x with Nothing" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    Just 42"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "replaceWithNothing")
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "Nothing"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 replaceWithNothing, got " ++ String.fromInt (List.length mutations))
            , test "replaces Just with a complex argument" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    Just (x + 1)"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "replaceWithNothing")
                    in
                    case mutations of
                        [ m ] ->
                            m.mutatedSource
                                |> String.contains "Nothing"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 replaceWithNothing, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "dropElseBranch"
            [ test "replaces else branch with then branch" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    if x then\n        \"yes\"\n    else\n        \"no\""

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "dropElseBranch")
                    in
                    case mutations of
                        [ m ] ->
                            -- The else branch "no" should be replaced with the then branch "yes"
                            let
                                lines =
                                    String.lines m.mutatedSource
                            in
                            -- Both the then and else branches should now say "yes"
                            lines
                                |> List.filter (String.contains "\"yes\"")
                                |> List.length
                                |> Expect.equal 2

                        _ ->
                            Expect.fail ("Expected 1 dropElseBranch, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "parse failure"
            [ test "returns empty list for invalid source" <|
                \_ ->
                    Mutator.generateMutations "not valid"
                        |> Expect.equal []
            ]
        ]
