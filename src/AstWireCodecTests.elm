module AstWireCodecTests exposing (suite)

import AstWireCodec
import Elm.Parser
import Elm.Syntax.File
import Expect
import Json.Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "AstWireCodec"
        [ test "roundtrips a representative normal module" <|
            \_ ->
                assertRoundtrip
                    """module Example exposing (Model, Msg(..), update)

import Basics exposing (Int)
import Html exposing (Html)

{-| Model docs -}
type alias Model =
    { count : Int }

type Msg
    = Inc
    | Dec

update : Msg -> Model -> Model
update msg model =
    case msg of
        Inc ->
            { model | count = model.count + 1 }

        Dec ->
            { model | count = model.count - 1 }
"""
        , test "roundtrips exposing all ranges" <|
            \_ ->
                assertRoundtrip
                    """module Foo exposing (..)

import Bar exposing (..)

-- a comment
foo : Int -> Int
foo x =
    x
"""
        , test "roundtrips a port module" <|
            \_ ->
                assertRoundtrip
                    """port module Ports exposing (..)

port send : String -> Cmd msg

port receive : (String -> msg) -> Sub msg
"""
        ]


assertRoundtrip : String -> Expect.Expectation
assertRoundtrip source =
    case Elm.Parser.parseToFile source of
        Err _ ->
            Expect.fail "Expected test source to parse"

        Ok file ->
            case AstWireCodec.decodeFromBytes (AstWireCodec.encodeToBytes file) of
                Nothing ->
                    Expect.fail "AstWireCodec.decodeFromBytes returned Nothing"

                Just decodedFile ->
                    decodedFile
                        |> Elm.Syntax.File.encode
                        |> Json.Encode.encode 0
                        |> Expect.equal
                            (file
                                |> Elm.Syntax.File.encode
                                |> Json.Encode.encode 0
                            )
