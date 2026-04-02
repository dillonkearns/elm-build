module SemanticHashTests exposing (suite)

import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Expect
import SemanticHash
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "SemanticHash"
        [ describe "hashExpression"
            [ test "same expression produces same hash" <|
                \_ ->
                    let
                        expr1 =
                            parseExpression "module A exposing (..)\n\nfoo = 42"

                        expr2 =
                            parseExpression "module B exposing (..)\n\nbar = 42"
                    in
                    case ( expr1, expr2 ) of
                        ( Just e1, Just e2 ) ->
                            SemanticHash.hashExpression e1
                                |> Expect.equal (SemanticHash.hashExpression e2)

                        _ ->
                            Expect.fail "Failed to parse"
            , test "different expressions produce different hashes" <|
                \_ ->
                    let
                        expr1 =
                            parseExpression "module A exposing (..)\n\nfoo = 42"

                        expr2 =
                            parseExpression "module A exposing (..)\n\nfoo = 43"
                    in
                    case ( expr1, expr2 ) of
                        ( Just e1, Just e2 ) ->
                            SemanticHash.hashExpression e1
                                |> Expect.notEqual (SemanticHash.hashExpression e2)

                        _ ->
                            Expect.fail "Failed to parse"
            , test "function references include qualified name" <|
                \_ ->
                    let
                        expr1 =
                            parseExpression "module A exposing (..)\n\nfoo x = MathLib.abs x"

                        expr2 =
                            parseExpression "module A exposing (..)\n\nfoo x = MathLib.sign x"
                    in
                    case ( expr1, expr2 ) of
                        ( Just e1, Just e2 ) ->
                            SemanticHash.hashExpression e1
                                |> Expect.notEqual (SemanticHash.hashExpression e2)

                        _ ->
                            Expect.fail "Failed to parse"
            , test "operator differences produce different hashes" <|
                \_ ->
                    let
                        expr1 =
                            parseExpression "module A exposing (..)\n\nfoo a b = a + b"

                        expr2 =
                            parseExpression "module A exposing (..)\n\nfoo a b = a - b"
                    in
                    case ( expr1, expr2 ) of
                        ( Just e1, Just e2 ) ->
                            SemanticHash.hashExpression e1
                                |> Expect.notEqual (SemanticHash.hashExpression e2)

                        _ ->
                            Expect.fail "Failed to parse"
            , test "if-else structure hashes differently from just the condition" <|
                \_ ->
                    let
                        expr1 =
                            parseExpression "module A exposing (..)\n\nfoo x = if x then 1 else 0"

                        expr2 =
                            parseExpression "module A exposing (..)\n\nfoo x = if x then 0 else 1"
                    in
                    case ( expr1, expr2 ) of
                        ( Just e1, Just e2 ) ->
                            SemanticHash.hashExpression e1
                                |> Expect.notEqual (SemanticHash.hashExpression e2)

                        _ ->
                            Expect.fail "Failed to parse"
            ]
        , describe "extractDependencies"
            [ test "extracts qualified function reference" <|
                \_ ->
                    let
                        deps =
                            parseDeps "module A exposing (..)\n\nfoo x = MathLib.abs x"
                    in
                    -- extractDependencies returns ALL FunctionOrValue refs
                    -- (including local vars like x — filtering is done at index level)
                    deps
                        |> List.filter (\( m, _ ) -> not (List.isEmpty m))
                        |> Expect.equal [ ( [ "MathLib" ], "abs" ) ]
            , test "extracts multiple qualified dependencies" <|
                \_ ->
                    let
                        deps =
                            parseDeps "module A exposing (..)\n\nfoo x = if x > 0 then MathLib.abs x else MathLib.sign x"
                    in
                    deps
                        |> List.filter (\( m, _ ) -> not (List.isEmpty m))
                        |> List.sort
                        |> Expect.equal [ ( [ "MathLib" ], "abs" ), ( [ "MathLib" ], "sign" ) ]
            , test "unqualified references included in raw output" <|
                \_ ->
                    let
                        deps =
                            parseDeps "module A exposing (..)\n\nfoo x = negate x"
                    in
                    deps
                        |> List.member ( [], "negate" )
                        |> Expect.equal True
            ]
        , describe "buildIndex"
            [ test "independent functions have independent semantic hashes" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nbar = 1\n\nbaz = 2"

                        index =
                            SemanticHash.buildIndexFromSource source

                        barHash =
                            SemanticHash.getSemanticHash index "bar"

                        bazHash =
                            SemanticHash.getSemanticHash index "baz"
                    in
                    case ( barHash, bazHash ) of
                        ( Just h1, Just h2 ) ->
                            h1 |> Expect.notEqual h2

                        _ ->
                            Expect.fail "Expected both hashes to exist"
            , test "changing a dependency changes the caller's semantic hash" <|
                \_ ->
                    let
                        source1 =
                            "module Foo exposing (..)\n\nhelper = 1\n\ncaller = helper + 1"

                        source2 =
                            "module Foo exposing (..)\n\nhelper = 2\n\ncaller = helper + 1"

                        callerHash1 =
                            SemanticHash.buildIndexFromSource source1
                                |> (\idx -> SemanticHash.getSemanticHash idx "caller")

                        callerHash2 =
                            SemanticHash.buildIndexFromSource source2
                                |> (\idx -> SemanticHash.getSemanticHash idx "caller")
                    in
                    case ( callerHash1, callerHash2 ) of
                        ( Just h1, Just h2 ) ->
                            h1 |> Expect.notEqual h2

                        _ ->
                            Expect.fail "Expected both hashes to exist"
            , test "changing an unrelated function does NOT change the caller's hash" <|
                \_ ->
                    let
                        source1 =
                            "module Foo exposing (..)\n\nhelper = 1\n\nunrelated = 99\n\ncaller = helper + 1"

                        source2 =
                            "module Foo exposing (..)\n\nhelper = 1\n\nunrelated = 100\n\ncaller = helper + 1"

                        callerHash1 =
                            SemanticHash.buildIndexFromSource source1
                                |> (\idx -> SemanticHash.getSemanticHash idx "caller")

                        callerHash2 =
                            SemanticHash.buildIndexFromSource source2
                                |> (\idx -> SemanticHash.getSemanticHash idx "caller")
                    in
                    case ( callerHash1, callerHash2 ) of
                        ( Just h1, Just h2 ) ->
                            h1 |> Expect.equal h2

                        _ ->
                            Expect.fail "Expected both hashes to exist"
            ]
        ]


{-| Parse source and extract the first function's body expression.
-}
parseExpression : String -> Maybe (Node Expression)
parseExpression source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            file.declarations
                |> List.filterMap
                    (\(Node _ decl) ->
                        case decl of
                            FunctionDeclaration func ->
                                Just (Node.value func.declaration |> .expression)

                            _ ->
                                Nothing
                    )
                |> List.head

        Err _ ->
            Nothing


{-| Parse source and extract dependencies from the first function.
-}
parseDeps : String -> List ( List String, String )
parseDeps source =
    case parseExpression source of
        Just expr ->
            SemanticHash.extractDependencies expr

        Nothing ->
            []
