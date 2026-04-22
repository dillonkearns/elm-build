module ProjectEnvWireCodecTest exposing (suite)

{-| Roundtrip test for the ProjectEnv wire codec (step 8b).

The full main→worker handoff path:

    env
        |> Eval.Module.toWireFields
        |> ProjectEnvWireCodec.encodeWireFields
        |> ship
        |> decode
        |> ProjectEnvWireCodec.decodeWireFields
        |> Maybe.map Eval.Module.fromWireFields

Each test builds a small ProjectEnv from inline source, runs the
roundtrip, and asserts that evaluating the same expression on the
original and the reconstructed env produces identical results.

-}

import Elm.Syntax.Expression
import Eval.Module
import Expect
import ProjectEnvWireCodec
import Test exposing (Test, describe, test)
import Types


suite : Test
suite =
    describe "ProjectEnvWireCodec roundtrip"
        [ roundTripPreservesEval "constant integer"
            "module Foo exposing (..)\n\nanswer : Int\nanswer = 42\n"
            "Foo"
            "answer"
        , roundTripPreservesEval "arithmetic"
            "module Foo exposing (..)\n\nanswer : Int\nanswer = 6 * 7\n"
            "Foo"
            "answer"
        , roundTripPreservesEval "function call"
            ("module Foo exposing (..)\n\n"
                ++ "double : Int -> Int\ndouble x = x * 2\n\n"
                ++ "answer : Int\nanswer = double 21\n"
            )
            "Foo"
            "answer"
        , roundTripPreservesEval "list operations"
            ("module Foo exposing (..)\n\n"
                ++ "answer : Int\nanswer = List.sum [1, 2, 3, 4, 5]\n"
            )
            "Foo"
            "answer"
        , roundTripPreservesEval "case expression"
            ("module Foo exposing (..)\n\n"
                ++ "describe : Maybe Int -> String\n"
                ++ "describe m =\n"
                ++ "    case m of\n"
                ++ "        Just n -> String.fromInt n\n"
                ++ "        Nothing -> \"none\"\n\n"
                ++ "answer : String\nanswer = describe (Just 7)\n"
            )
            "Foo"
            "answer"
        ]


roundTripPreservesEval : String -> String -> String -> String -> Test
roundTripPreservesEval label src moduleName valueName =
    test label <|
        \_ ->
            case Eval.Module.buildProjectEnv [ src ] of
                Err err ->
                    Expect.fail ("buildProjectEnv failed: " ++ errToString err)

                Ok original ->
                    let
                        wireFields =
                            Eval.Module.toWireFields original

                        roundtrippedWire =
                            wireFields
                                |> ProjectEnvWireCodec.encodeWireFields
                                |> ProjectEnvWireCodec.decodeWireFields
                    in
                    case roundtrippedWire of
                        Nothing ->
                            Expect.fail "decodeWireFields returned Nothing"

                        Just decoded ->
                            let
                                reconstructed =
                                    Eval.Module.fromWireFields decoded

                                expression =
                                    Elm.Syntax.Expression.FunctionOrValue [ moduleName ] valueName

                                originalResult =
                                    Eval.Module.evalWithEnv original [] expression

                                reconstructedResult =
                                    Eval.Module.evalWithEnv reconstructed [] expression
                            in
                            Expect.equal
                                (resultToString originalResult)
                                (resultToString reconstructedResult)


resultToString : Result Types.Error Types.Value -> String
resultToString r =
    case r of
        Ok value ->
            "ok:" ++ Debug.toString value

        Err err ->
            "err:" ++ errToString err


errToString : Types.Error -> String
errToString err =
    case err of
        Types.ParsingError _ ->
            "parse error"

        Types.EvalError data ->
            "eval error in " ++ String.join "." data.currentModule ++ ": " ++ Debug.toString data.error
