module AstWireCodecTests exposing (suite)

import AstWireCodec
import Elm.Parser
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.File
import Elm.Syntax.Node as Node exposing (Node(..))
import Expect
import Json.Decode
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
        , test "elm-syntax JSON roundtrips open type imports" <|
            \_ ->
                case
                    Elm.Parser.parseToFile
                        """module Example exposing (..)

import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
"""
                of
                    Err _ ->
                        Expect.fail "Expected test source to parse"

                    Ok file ->
                        case
                            file
                                |> Elm.Syntax.File.encode
                                |> Json.Encode.encode 0
                                |> Json.Decode.decodeString Elm.Syntax.File.decoder
                        of
                            Err err ->
                                Expect.fail ("Expected JSON roundtrip to decode, but got: " ++ Json.Decode.errorToString err)

                            Ok decodedFile ->
                                decodedFile.imports
                                    |> List.filterMap
                                        (\(Node _ import_) ->
                                            if Node.value import_.moduleName == [ "Elm", "Syntax", "TypeAnnotation" ] then
                                                import_.exposingList

                                            else
                                                Nothing
                                        )
                                    |> List.head
                                    |> Maybe.andThen
                                        (\(Node _ exposing_) ->
                                            case exposing_ of
                                                Explicit exposes ->
                                                    exposes
                                                        |> List.filterMap
                                                            (\(Node _ expose) ->
                                                                case expose of
                                                                    TypeExpose exposedType ->
                                                                        Just exposedType.open

                                                                    TypeOrAliasExpose name ->
                                                                        if name == "TypeAnnotation" then
                                                                            Just Nothing

                                                                        else
                                                                            Nothing

                                                                    _ ->
                                                                        Nothing
                                                            )
                                                        |> List.head

                                                All _ ->
                                                    Nothing
                                        )
                                    |> Expect.notEqual Nothing
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
