module ValueWireCodec exposing (decodeValue, encodeValue, roundTrip)

import Array
import FastDict
import Lamdera.Wire3 as W
import Types exposing (Value(..))


encodeValue : Value -> W.Encoder
encodeValue value =
    case value of
        String s ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 0, W.encodeString s ]

        Int n ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 1, W.encodeInt n ]

        Float f ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 2, W.encodeFloat64 f ]

        Char c ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 3, W.encodeChar c ]

        Bool b ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 4, W.encodeBool b ]

        Unit ->
            W.encodeUnsignedInt8 5

        Tuple a b ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 6, encodeValue a, encodeValue b ]

        Triple a b c ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 7, encodeValue a, encodeValue b, encodeValue c ]

        Record fields ->
            W.encodeSequenceWithoutLength
                [ W.encodeUnsignedInt8 8
                , W.encodeList
                    (\( k, v ) ->
                        W.encodeSequenceWithoutLength [ W.encodeString k, encodeValue v ]
                    )
                    (FastDict.toList fields)
                ]

        Custom ref args ->
            W.encodeSequenceWithoutLength
                [ W.encodeUnsignedInt8 9
                , W.encodeList W.encodeString ref.moduleName
                , W.encodeString ref.name
                , W.encodeList encodeValue args
                ]

        List items ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 10, W.encodeList encodeValue items ]

        JsArray arr ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 11, W.encodeList encodeValue (Array.toList arr) ]

        _ ->
            W.encodeUnsignedInt8 255


decodeValue : W.Decoder Value
decodeValue =
    W.decodeUnsignedInt8
        |> W.andThenDecode
            (\tag ->
                case tag of
                    0 ->
                        W.decodeString |> mapDecode String

                    1 ->
                        W.decodeInt |> mapDecode Int

                    2 ->
                        W.decodeFloat64 |> mapDecode Float

                    3 ->
                        W.decodeChar |> mapDecode Char

                    4 ->
                        W.decodeBool |> mapDecode Bool

                    5 ->
                        W.succeedDecode Unit

                    6 ->
                        W.succeedDecode Tuple
                            |> W.andMapDecode decodeValue
                            |> W.andMapDecode decodeValue

                    7 ->
                        W.succeedDecode Triple
                            |> W.andMapDecode decodeValue
                            |> W.andMapDecode decodeValue
                            |> W.andMapDecode decodeValue

                    8 ->
                        W.decodeList
                            (W.succeedDecode Tuple.pair
                                |> W.andMapDecode W.decodeString
                                |> W.andMapDecode decodeValue
                            )
                            |> mapDecode FastDict.fromList
                            |> mapDecode Record

                    9 ->
                        W.succeedDecode
                            (\modName name args ->
                                Custom { moduleName = modName, name = name } args
                            )
                            |> W.andMapDecode (W.decodeList W.decodeString)
                            |> W.andMapDecode W.decodeString
                            |> W.andMapDecode (W.decodeList decodeValue)

                    10 ->
                        W.decodeList decodeValue |> mapDecode List

                    11 ->
                        W.decodeList decodeValue |> mapDecode (Array.fromList >> JsArray)

                    _ ->
                        W.failDecode
            )


roundTrip : Value -> Maybe Value
roundTrip value =
    encodeValue value
        |> W.bytesEncode
        |> W.bytesDecode decodeValue


mapDecode : (a -> b) -> W.Decoder a -> W.Decoder b
mapDecode f decoder =
    W.succeedDecode f |> W.andMapDecode decoder
