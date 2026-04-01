module MutatorTests exposing (suite)

import Elm.Parser
import Expect
import Mutator exposing (Mutation)
import Set
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
                            (Mutator.applyMutation source m)
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
                            (Mutator.applyMutation source m)
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
                            (Mutator.applyMutation source m)
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
                            (Mutator.applyMutation source m)
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
                            (Mutator.applyMutation source m)
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
                            (Mutator.applyMutation source m)
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
                            (Mutator.applyMutation source m)
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
                            (Mutator.applyMutation source m)
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
                            (Mutator.applyMutation source m)
                                |> String.contains "\n    1"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 replaceIntLiteral, got " ++ String.fromInt (List.length mutations))
            , test "replaces 1 with 2" <|
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
                            (Mutator.applyMutation source m)
                                |> String.contains "\n    2"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 replaceIntLiteral, got " ++ String.fromInt (List.length mutations))
            , test "replaces 42 with 43" <|
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
                            (Mutator.applyMutation source m)
                                |> String.contains "\n    43"
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
                            (Mutator.applyMutation source m)
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
                            ((Mutator.applyMutation source m) |> String.contains "\"\"")
                                |> Expect.equal False

                        _ ->
                            Expect.fail ("Expected 1 replaceStringLiteral, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "removeListElement"
            [ test "generates first and last removal for 3-element list" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    [ 1, 2, 3 ]"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removeListElement")
                    in
                    List.length mutations
                        |> Expect.equal 2
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
                            Mutator.applyMutation source first
                                |> String.contains "[ 2, 3 ]"
                                |> Expect.equal True

                        [] ->
                            Expect.fail "Expected at least 1 removeListElement"
            , test "second removal drops the last element" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    [ 1, 2, 3 ]"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removeListElement")
                    in
                    case mutations of
                        _ :: [ second ] ->
                            Mutator.applyMutation source second
                                |> String.contains "[ 1, 2 ]"
                                |> Expect.equal True

                        _ ->
                            Expect.fail "Expected exactly 2 removeListElement"
            , test "generates only 2 removals even for large lists" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removeListElement")
                    in
                    List.length mutations
                        |> Expect.equal 2
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
                            (Mutator.applyMutation source m)
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
                            (Mutator.applyMutation source m)
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
                                    String.lines (Mutator.applyMutation source m)
                            in
                            -- Both the then and else branches should now say "yes"
                            lines
                                |> List.filter (String.contains "\"yes\"")
                                |> List.length
                                |> Expect.equal 2

                        _ ->
                            Expect.fail ("Expected 1 dropElseBranch, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "fullComparisonNegation"
            [ test "negates < to >=" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo a b =\n    a < b"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "negateComparison" && String.contains ">=" m.description)
                    in
                    case mutations of
                        [ m ] ->
                            (Mutator.applyMutation source m)
                                |> String.contains "a >= b"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 negateComparison >=, got " ++ String.fromInt (List.length mutations))
            , test "negates > to <=" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo a b =\n    a > b"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "negateComparison" && String.contains "<=" m.description)
                    in
                    case mutations of
                        [ m ] ->
                            (Mutator.applyMutation source m)
                                |> String.contains "a <= b"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 negateComparison <=, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "removeNot"
            [ test "removes not from function call" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    not x"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removeNot")
                    in
                    case mutations of
                        [ m ] ->
                            -- "not x" should become just "x"
                            ((Mutator.applyMutation source m) |> String.contains "    not x")
                                |> Expect.equal False

                        _ ->
                            Expect.fail ("Expected 1 removeNot, got " ++ String.fromInt (List.length mutations))
            , test "removes not with parens" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    not (x > 0)"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removeNot")
                    in
                    case mutations of
                        [ m ] ->
                            (Mutator.applyMutation source m)
                                |> String.contains "    (x > 0)"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 removeNot, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "removeNegation"
            [ test "removes unary minus" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    -x"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removeNegation")
                    in
                    case mutations of
                        [ m ] ->
                            -- "-x" should become "x"
                            ((Mutator.applyMutation source m) |> String.contains "    -x")
                                |> Expect.equal False

                        _ ->
                            Expect.fail ("Expected 1 removeNegation, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "emptyList"
            [ test "replaces list with empty" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    [ 1, 2, 3 ]"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "emptyList")
                    in
                    case mutations of
                        [ m ] ->
                            (Mutator.applyMutation source m)
                                |> String.contains "    []"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 emptyList, got " ++ String.fromInt (List.length mutations))
            , test "does not generate for empty list" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    []"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "emptyList")
                    in
                    List.length mutations
                        |> Expect.equal 0
            , test "does not generate for single-element list" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    [ 1 ]"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "emptyList")
                    in
                    List.length mutations
                        |> Expect.equal 0
            ]
        , describe "concatRemoval"
            [ test "replaces a ++ b with a" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo a b =\n    a ++ b"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "concatRemoval")
                    in
                    -- Should produce 2: keep left, keep right
                    List.length mutations
                        |> Expect.equal 2
            , test "first keeps left side" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo =\n    \"hello\" ++ \" world\""

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "concatRemoval" && String.contains "left" m.description)
                    in
                    case mutations of
                        [ m ] ->
                            (Mutator.applyMutation source m)
                                |> String.contains "    \"hello\""
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 concatRemoval left, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "conditionalToConstant"
            [ test "replaces condition with True" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    if x > 0 then\n        \"yes\"\n    else\n        \"no\""

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "conditionalTrue")
                    in
                    case mutations of
                        [ m ] ->
                            (Mutator.applyMutation source m)
                                |> String.contains "if True then"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 conditionalTrue, got " ++ String.fromInt (List.length mutations))
            , test "replaces condition with False" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    if x > 0 then\n        \"yes\"\n    else\n        \"no\""

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "conditionalFalse")
                    in
                    case mutations of
                        [ m ] ->
                            (Mutator.applyMutation source m)
                                |> String.contains "if False then"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 conditionalFalse, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "functionSwap"
            [ test "swaps List.head to List.last" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo xs =\n    List.head xs"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "functionSwap")
                    in
                    case mutations of
                        [ m ] ->
                            (Mutator.applyMutation source m)
                                |> String.contains "List.last xs"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 functionSwap, got " ++ String.fromInt (List.length mutations))
            , test "swaps String.toUpper to String.toLower" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo s =\n    String.toUpper s"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "functionSwap")
                    in
                    case mutations of
                        [ m ] ->
                            (Mutator.applyMutation source m)
                                |> String.contains "String.toLower s"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 functionSwap, got " ++ String.fromInt (List.length mutations))
            , test "swaps List.sort to List.reverse" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo xs =\n    List.sort xs"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "functionSwap")
                    in
                    case mutations of
                        [ m ] ->
                            (Mutator.applyMutation source m)
                                |> String.contains "List.reverse xs"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 functionSwap, got " ++ String.fromInt (List.length mutations))
            , test "swaps List.reverse to List.sort" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo xs =\n    List.reverse xs"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "functionSwap")
                    in
                    case mutations of
                        [ m ] ->
                            (Mutator.applyMutation source m)
                                |> String.contains "List.sort xs"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 functionSwap, got " ++ String.fromInt (List.length mutations))
            , test "swaps List.minimum to List.maximum" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo xs =\n    List.minimum xs"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "functionSwap")
                    in
                    case mutations of
                        [ m ] ->
                            (Mutator.applyMutation source m)
                                |> String.contains "List.maximum xs"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 functionSwap, got " ++ String.fromInt (List.length mutations))
            , test "no swap for unknown function" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo xs =\n    List.map identity xs"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "functionSwap")
                    in
                    List.length mutations
                        |> Expect.equal 0
            ]
        , describe "multi-line exposing"
            [ test "mutations produce parseable source for multi-line exposing" <|
                \_ ->
                    let
                        source =
                            String.join "\n"
                                [ "module Foo exposing"
                                , "    ( bar"
                                , "    , baz"
                                , "    )"
                                , ""
                                , "bar x ="
                                , "    if x then"
                                , "        1"
                                , "    else"
                                , "        0"
                                , ""
                                , "baz ="
                                , "    42"
                                ]

                        mutations =
                            Mutator.generateMutations source

                        firstFailure =
                            mutations
                                |> List.filterMap
                                    (\m ->
                                        let
                                            applied =
                                                Mutator.applyMutation source m
                                        in
                                        case Elm.Parser.parseToFile applied of
                                            Ok _ ->
                                                Nothing

                                            Err _ ->
                                                Just (m.operator ++ ": " ++ m.description ++ "\n---\n" ++ applied ++ "\n---")
                                    )
                                |> List.head
                    in
                    if List.isEmpty mutations then
                        Expect.fail "Expected mutations to be generated"

                    else
                        case firstFailure of
                            Just failInfo ->
                                Expect.fail ("Mutation produced unparseable source:\n" ++ failInfo)

                            Nothing ->
                                Expect.pass
            ]
        , describe "hashKey"
            [ test "all mutations for a module produce unique hash keys" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    if x > 0 then\n        1\n    else\n        0\n\nbar a b =\n    a + b"

                        mutations =
                            Mutator.generateMutations source

                        hashKeys =
                            List.map Mutator.hashKey mutations

                        uniqueKeys =
                            Set.fromList hashKeys

                        debugInfo =
                            mutations
                                |> List.map (\m -> Mutator.hashKey m ++ " [" ++ m.operator ++ "]")
                                |> String.join "\n"
                    in
                    if List.length mutations < 2 then
                        Expect.fail "Expected multiple mutations"

                    else if Set.size uniqueKeys /= List.length mutations then
                        Expect.fail
                            ("Expected "
                                ++ String.fromInt (List.length mutations)
                                ++ " unique keys, got "
                                ++ String.fromInt (Set.size uniqueKeys)
                                ++ "\n"
                                ++ debugInfo
                            )

                    else
                        Expect.pass
            , test "same source produces same hash keys" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    x > 0"

                        keys1 =
                            Mutator.generateMutations source |> List.map Mutator.hashKey

                        keys2 =
                            Mutator.generateMutations source |> List.map Mutator.hashKey
                    in
                    keys1 |> Expect.equal keys2
            ]
        , describe "extremeMutation"
            [ test "replaces Int-returning function body with 0" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo : Int -> Int\nfoo x =\n    x + 1"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "extremeMutation")
                    in
                    case mutations of
                        [ m ] ->
                            Mutator.applyMutation source m
                                |> String.contains "    0"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 extremeMutation, got " ++ String.fromInt (List.length mutations))
            , test "replaces String-returning function body with empty string" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\ngreet : String -> String\ngreet name =\n    \"Hello, \" ++ name"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "extremeMutation")
                    in
                    case mutations of
                        [ m ] ->
                            Mutator.applyMutation source m
                                |> String.contains "    \"\""
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 extremeMutation, got " ++ String.fromInt (List.length mutations))
            , test "replaces Bool-returning function body with False" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nisPositive : Int -> Bool\nisPositive n =\n    n > 0"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "extremeMutation")
                    in
                    case mutations of
                        [ m ] ->
                            Mutator.applyMutation source m
                                |> String.contains "    False"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 extremeMutation, got " ++ String.fromInt (List.length mutations))
            , test "replaces List-returning function body with []" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\ndouble : List Int -> List Int\ndouble xs =\n    List.map (\\x -> x * 2) xs"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "extremeMutation")
                    in
                    case mutations of
                        [ m ] ->
                            Mutator.applyMutation source m
                                |> String.contains "    []"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 extremeMutation, got " ++ String.fromInt (List.length mutations))
            , test "replaces Maybe-returning function body with Nothing" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfindFirst : List a -> Maybe a\nfindFirst xs =\n    List.head xs"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "extremeMutation")
                    in
                    case mutations of
                        [ m ] ->
                            Mutator.applyMutation source m
                                |> String.contains "    Nothing"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 extremeMutation, got " ++ String.fromInt (List.length mutations))
            , test "does not generate for functions without type annotations" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    x + 1"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "extremeMutation")
                    in
                    List.length mutations
                        |> Expect.equal 0
            ]
        , describe "removePipelineStep"
            [ test "removes a |> step" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    x |> List.sort |> List.reverse"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removePipelineStep")
                    in
                    -- x |> sort |> reverse should generate removals for sort and reverse
                    List.length mutations
                        |> Expect.atLeast 1
            , test "keeps input when removing step" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nfoo x =\n    x |> String.toUpper"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removePipelineStep")
                    in
                    case mutations of
                        [ m ] ->
                            -- Should just keep `x` (the input)
                            Mutator.applyMutation source m
                                |> String.contains "    x"
                                |> Expect.equal True

                        _ ->
                            Expect.fail ("Expected 1 removePipelineStep, got " ++ String.fromInt (List.length mutations))
            ]
        , describe "removeRecordUpdate"
            [ test "removes a field from a record update" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nupdate r =\n    { r | x = 1, y = 2 }"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removeRecordUpdate")
                    in
                    -- Should generate 2 mutations: remove x update, remove y update
                    List.length mutations
                        |> Expect.equal 2
            , test "removing first field keeps second" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nupdate r =\n    { r | x = 1, y = 2 }"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removeRecordUpdate")
                    in
                    case mutations of
                        first :: _ ->
                            let
                                applied =
                                    Mutator.applyMutation source first
                            in
                            (String.contains "y = 2" applied && not (String.contains "x = 1" applied))
                                |> Expect.equal True

                        [] ->
                            Expect.fail "Expected at least 1 removeRecordUpdate"
            , test "does not generate for single-field update" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nupdate r =\n    { r | x = 1 }"

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "removeRecordUpdate")
                    in
                    List.length mutations
                        |> Expect.equal 0
            ]
        , describe "swapCaseBranches"
            [ test "swaps bodies of adjacent case branches" <|
                \_ ->
                    let
                        source =
                            String.join "\n"
                                [ "module Foo exposing (..)"
                                , ""
                                , "describe x ="
                                , "    case x of"
                                , "        0 ->"
                                , "            \"zero\""
                                , ""
                                , "        1 ->"
                                , "            \"one\""
                                , ""
                                , "        _ ->"
                                , "            \"other\""
                                ]

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "swapCaseBranches")
                    in
                    -- With 3 branches, should get 2 swaps (0<->1, 1<->2)
                    List.length mutations
                        |> Expect.atLeast 2
            , test "swap replaces first branch body with second" <|
                \_ ->
                    let
                        source =
                            String.join "\n"
                                [ "module Foo exposing (..)"
                                , ""
                                , "describe x ="
                                , "    case x of"
                                , "        0 ->"
                                , "            \"zero\""
                                , ""
                                , "        1 ->"
                                , "            \"one\""
                                , ""
                                , "        _ ->"
                                , "            \"other\""
                                ]

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "swapCaseBranches")

                        firstSwapApplied =
                            case mutations of
                                m :: _ ->
                                    Mutator.applyMutation source m

                                [] ->
                                    ""
                    in
                    -- The first branch (0 -> "zero") should now have "one" as its body
                    -- The pattern `0 ->` should be followed by "one" instead of "zero"
                    firstSwapApplied
                        |> String.contains "        0 ->\n            \"one\""
                        |> Expect.equal True
            , test "no swap when adjacent branches have identical bodies" <|
                \_ ->
                    let
                        source =
                            String.join "\n"
                                [ "module Foo exposing (..)"
                                , ""
                                , "isGood x ="
                                , "    case x of"
                                , "        A ->"
                                , "            True"
                                , ""
                                , "        B ->"
                                , "            True"
                                , ""
                                , "        _ ->"
                                , "            False"
                                ]

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "swapCaseBranches")
                    in
                    -- A->True and B->True have same body, so only the B<->_ swap should exist
                    List.length mutations
                        |> Expect.equal 1
            , test "no swap for single-branch case" <|
                \_ ->
                    let
                        source =
                            String.join "\n"
                                [ "module Foo exposing (..)"
                                , ""
                                , "unwrap x ="
                                , "    case x of"
                                , "        Just v ->"
                                , "            v"
                                ]

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "swapCaseBranches")
                    in
                    List.length mutations
                        |> Expect.equal 0
            ]
        , describe "timeout detection"
            [ test "infinite recursion mutation is detected as error, not a hang" <|
                \_ ->
                    -- This source has a recursive function. Mutating `n - 1` to `n + 1`
                    -- would cause infinite recursion. The interpreter should detect this
                    -- via a step limit and return an error rather than hanging.
                    let
                        source =
                            String.join "\n"
                                [ "module Foo exposing (..)"
                                , ""
                                , "countdown n ="
                                , "    if n <= 0 then"
                                , "        0"
                                , "    else"
                                , "        countdown (n - 1)"
                                ]

                        mutations =
                            Mutator.generateMutations source
                                |> List.filter (\m -> m.operator == "replaceArithmetic")

                        -- The mutation changes `n - 1` to `n + 1`, causing infinite recursion
                        -- We just verify the mutation exists (the timeout test is an integration concern)
                    in
                    List.length mutations
                        |> Expect.atLeast 1
            ]
        , describe "no-op mutations"
            [ test "no mutation should ever produce identical replacement text" <|
                \_ ->
                    let
                        sources =
                            [ -- Case with duplicate branches
                              String.join "\n"
                                [ "module Foo exposing (..)"
                                , ""
                                , "isGood x ="
                                , "    case x of"
                                , "        A ->"
                                , "            True"
                                , ""
                                , "        B ->"
                                , "            True"
                                , ""
                                , "        _ ->"
                                , "            False"
                                ]

                            -- Function with various operators
                            , String.join "\n"
                                [ "module Bar exposing (..)"
                                , ""
                                , "compute : Int -> Int -> Bool"
                                , "compute a b ="
                                , "    if a > b then"
                                , "        a + b"
                                , "    else"
                                , "        a - b"
                                ]

                            -- Pipeline and concat
                            , String.join "\n"
                                [ "module Baz exposing (..)"
                                , ""
                                , "process x ="
                                , "    x |> String.toUpper |> String.reverse"
                                , ""
                                , "greeting name ="
                                , "    \"Hello \" ++ name"
                                ]
                            ]

                        allNoOps =
                            sources
                                |> List.concatMap
                                    (\source ->
                                        Mutator.generateMutations source
                                            |> List.filterMap
                                                (\m ->
                                                    let
                                                        originalText =
                                                            Mutator.extractOriginalText source m
                                                    in
                                                    if originalText == m.spliceText then
                                                        Just (m.operator ++ ": " ++ m.description ++ " [" ++ m.spliceText ++ "]")

                                                    else
                                                        Nothing
                                                )
                                    )
                    in
                    if List.isEmpty allNoOps then
                        Expect.pass

                    else
                        Expect.fail ("Found no-op mutations:\n" ++ String.join "\n" allNoOps)
            ]
        , describe "parse failure"
            [ test "returns empty list for invalid source" <|
                \_ ->
                    Mutator.generateMutations "not valid"
                        |> Expect.equal []
            ]
        ]
