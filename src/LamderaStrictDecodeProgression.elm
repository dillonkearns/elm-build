module LamderaStrictDecodeProgression exposing (run)

import AstWireCodec
import BackendTask exposing (BackendTask)
import Bytes
import Elm.Parser
import Elm.Syntax.File
import Elm.Syntax.Node
import FatalError exposing (FatalError)
import Lamdera.Wire3 as W
import Pages.Script as Script exposing (Script)


type alias Checkpoint =
    { name : String
    , encodedBytes : Int
    , lenientOk : Bool
    , strictOk : Bool
    }


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    case parseFixture of
        Err message ->
            BackendTask.fail (FatalError.fromString message)

        Ok file ->
            let
                checkpoints =
                    buildCheckpoints file

                completed =
                    collectUntilFirstStrictFailure checkpoints
            in
            completed
                |> formatReport
                |> Script.log
                |> BackendTask.allowFatal


parseFixture : Result String Elm.Syntax.File.File
parseFixture =
    Elm.Parser.parseToFile
        """module Example exposing (..)

import Basics exposing (Int)

-- a comment
type Msg
    = Inc

foo : Int
foo =
    1
"""
        |> Result.mapError (\_ -> "Failed to parse inline fixture source")


buildCheckpoints : Elm.Syntax.File.File -> List (() -> Checkpoint)
buildCheckpoints file =
    let
        headerNode =
            file.moduleDefinition

        firstImportNode =
            List.head file.imports

        firstDeclarationNode =
            List.head file.declarations

        firstCommentNode =
            List.head file.comments
    in
    [ checkpointThunk "primitive:int" (W.encodeInt 42) W.decodeInt
    , checkpointThunk "primitive:string" (W.encodeString "hello") W.decodeString
    , checkpointThunk "primitive:list<string>" (W.encodeList W.encodeString [ "a", "b" ]) (W.decodeList W.decodeString)
    , checkpointThunk "ast:moduleDefinition" (AstWireCodec.encodeNode AstWireCodec.encodeModule headerNode) (AstWireCodec.decodeNode AstWireCodec.decodeModule)
    ]
        ++ maybeCheckpoint "ast:firstImport" firstImportNode (AstWireCodec.encodeNode AstWireCodec.encodeImport) (AstWireCodec.decodeNode AstWireCodec.decodeImport)
        ++ maybeCheckpoint "ast:firstDeclaration" firstDeclarationNode (AstWireCodec.encodeNode AstWireCodec.encodeDeclaration) (AstWireCodec.decodeNode AstWireCodec.decodeDeclaration)
        ++ maybeCheckpoint "ast:firstComment" firstCommentNode (AstWireCodec.encodeNode W.encodeString) (AstWireCodec.decodeNode W.decodeString)
        ++ [ checkpointThunk "ast:file" (AstWireCodec.encodeFile file) AstWireCodec.decodeFile ]


checkpoint : String -> W.Encoder -> W.Decoder a -> Checkpoint
checkpoint name encoder decoder =
    let
        bytes =
            W.bytesEncode encoder
    in
    { name = name
    , encodedBytes = Bytes.width bytes
    , lenientOk = W.bytesDecode decoder bytes /= Nothing
    , strictOk = W.bytesDecodeStrict decoder bytes /= Nothing
    }


checkpointThunk : String -> W.Encoder -> W.Decoder a -> () -> Checkpoint
checkpointThunk name encoder decoder () =
    checkpoint name encoder decoder


maybeCheckpoint : String -> Maybe a -> (a -> W.Encoder) -> W.Decoder a -> List (() -> Checkpoint)
maybeCheckpoint name maybeValue encoder decoder =
    case maybeValue of
        Nothing ->
            []

        Just value ->
            [ checkpointThunk name (encoder value) decoder ]


collectUntilFirstStrictFailure : List (() -> Checkpoint) -> List Checkpoint
collectUntilFirstStrictFailure checkpointThunks =
    case checkpointThunks of
        [] ->
            []

        checkpointThunk_ :: rest ->
            let
                checkpoint_ =
                    checkpointThunk_ ()
            in
            if checkpoint_.strictOk then
                checkpoint_ :: collectUntilFirstStrictFailure rest

            else
                [ checkpoint_ ]


formatReport : List Checkpoint -> String
formatReport checkpoints =
    let
        firstFailure =
            checkpoints
                |> List.filter (\checkpoint_ -> not checkpoint_.strictOk)
                |> List.head
                |> Maybe.map .name
                |> Maybe.withDefault "none"
    in
    String.join "\n"
        ([ "Lamdera strict decode progression"
         , "  first strict failure: " ++ firstFailure
         , ""
         ]
            ++ List.map formatCheckpoint checkpoints
        )


formatCheckpoint : Checkpoint -> String
formatCheckpoint checkpoint_ =
    "  - "
        ++ checkpoint_.name
        ++ " bytes="
        ++ String.fromInt checkpoint_.encodedBytes
        ++ " lenient="
        ++ boolToString checkpoint_.lenientOk
        ++ " strict="
        ++ boolToString checkpoint_.strictOk


boolToString : Bool -> String
boolToString value =
    if value then
        "true"

    else
        "false"
