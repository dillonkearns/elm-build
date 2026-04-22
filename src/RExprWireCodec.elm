module RExprWireCodec exposing
    ( decodeBodies
    , decodeRExpr
    , decodeRPattern
    , encodeBodies
    , encodeRExpr
    , encodeRPattern
    )

{-| Hand-rolled Wire3 codec for `Eval.ResolvedIR` types — RExpr, RPattern,
RLetBinding, GlobalId — and the `bodies : Dict GlobalId RExpr` field of
`ResolvedProject`.

Built for the step-8a thin-slice experiment in the parallel-ceiling
investigation: measure encoded byte size + decode throughput on a real
project's resolved IR before committing to a full ProjectEnv codec.
Tier-2's prior attempt at shipping File ASTs hit ~2.4 MB/s decode and
regressed by +4-6 s; this experiment confirms whether RExpr is in the
same throughput regime or denser.

Pattern follows `AstWireCodec` and `ValueWireCodec`: tagged union with
one-byte discriminator, recursive encoders for variant payloads.

-}

import Bytes exposing (Bytes)
import Eval.ResolvedIR as IR exposing (RExpr(..), RLetBinding, RPattern(..))
import FastDict
import Lamdera.Wire3 as W



-- BODIES (top-level entry)


encodeBodies : FastDict.Dict IR.GlobalId RExpr -> Bytes
encodeBodies bodies =
    bodies
        |> FastDict.toList
        |> W.encodeList encodeBodyEntry
        |> W.bytesEncode


decodeBodies : Bytes -> Maybe (FastDict.Dict IR.GlobalId RExpr)
decodeBodies bytes =
    W.bytesDecode (W.decodeList decodeBodyEntry |> mapDecode FastDict.fromList) bytes


encodeBodyEntry : ( IR.GlobalId, RExpr ) -> W.Encoder
encodeBodyEntry ( id, expr ) =
    W.encodeSequenceWithoutLength
        [ W.encodeInt id
        , encodeRExpr expr
        ]


decodeBodyEntry : W.Decoder ( IR.GlobalId, RExpr )
decodeBodyEntry =
    W.succeedDecode Tuple.pair
        |> W.andMapDecode W.decodeInt
        |> W.andMapDecode decodeRExpr



-- RExpr


encodeRExpr : RExpr -> W.Encoder
encodeRExpr expr =
    case expr of
        RInt n ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 0, W.encodeInt n ]

        RFloat f ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 1, W.encodeFloat64 f ]

        RString s ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 2, W.encodeString s ]

        RChar c ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 3, W.encodeChar c ]

        RUnit ->
            W.encodeUnsignedInt8 4

        RLocal i ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 5, W.encodeInt i ]

        RGlobal id ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 6, W.encodeInt id ]

        RCtor ref ->
            W.encodeSequenceWithoutLength
                [ W.encodeUnsignedInt8 7
                , W.encodeList W.encodeString ref.moduleName
                , W.encodeString ref.name
                ]

        RRecordAccessFunction name ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 8, W.encodeString name ]

        RIf c t e ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 9, encodeRExpr c, encodeRExpr t, encodeRExpr e ]

        RAnd a b ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 10, encodeRExpr a, encodeRExpr b ]

        ROr a b ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 11, encodeRExpr a, encodeRExpr b ]

        RCase scrut branches ->
            W.encodeSequenceWithoutLength
                [ W.encodeUnsignedInt8 12
                , encodeRExpr scrut
                , W.encodeList encodeBranch branches
                ]

        RLambda payload ->
            W.encodeSequenceWithoutLength
                [ W.encodeUnsignedInt8 13
                , W.encodeInt payload.arity
                , encodeRExpr payload.body
                , W.encodeList W.encodeInt payload.captureSlots
                ]

        RLet bindings body ->
            W.encodeSequenceWithoutLength
                [ W.encodeUnsignedInt8 14
                , W.encodeList encodeRLetBinding bindings
                , encodeRExpr body
                ]

        RApply fn args ->
            W.encodeSequenceWithoutLength
                [ W.encodeUnsignedInt8 15
                , encodeRExpr fn
                , W.encodeList encodeRExpr args
                ]

        RNegate inner ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 16, encodeRExpr inner ]

        RList items ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 17, W.encodeList encodeRExpr items ]

        RTuple2 a b ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 18, encodeRExpr a, encodeRExpr b ]

        RTuple3 a b c ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 19, encodeRExpr a, encodeRExpr b, encodeRExpr c ]

        RRecord fields ->
            W.encodeSequenceWithoutLength
                [ W.encodeUnsignedInt8 20
                , W.encodeList encodeRecordField fields
                ]

        RRecordAccess inner field ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 21, encodeRExpr inner, W.encodeString field ]

        RRecordUpdate slot fields ->
            W.encodeSequenceWithoutLength
                [ W.encodeUnsignedInt8 22
                , W.encodeInt slot
                , W.encodeList encodeRecordField fields
                ]

        RGLSL ->
            W.encodeUnsignedInt8 23


decodeRExpr : W.Decoder RExpr
decodeRExpr =
    W.decodeUnsignedInt8
        |> W.andThenDecode
            (\tag ->
                case tag of
                    0 ->
                        W.decodeInt |> mapDecode RInt

                    1 ->
                        W.decodeFloat64 |> mapDecode RFloat

                    2 ->
                        W.decodeString |> mapDecode RString

                    3 ->
                        W.decodeChar |> mapDecode RChar

                    4 ->
                        W.succeedDecode RUnit

                    5 ->
                        W.decodeInt |> mapDecode RLocal

                    6 ->
                        W.decodeInt |> mapDecode RGlobal

                    7 ->
                        W.succeedDecode (\m n -> RCtor { moduleName = m, name = n })
                            |> W.andMapDecode (W.decodeList W.decodeString)
                            |> W.andMapDecode W.decodeString

                    8 ->
                        W.decodeString |> mapDecode RRecordAccessFunction

                    9 ->
                        W.succeedDecode RIf
                            |> W.andMapDecode decodeRExpr
                            |> W.andMapDecode decodeRExpr
                            |> W.andMapDecode decodeRExpr

                    10 ->
                        W.succeedDecode RAnd
                            |> W.andMapDecode decodeRExpr
                            |> W.andMapDecode decodeRExpr

                    11 ->
                        W.succeedDecode ROr
                            |> W.andMapDecode decodeRExpr
                            |> W.andMapDecode decodeRExpr

                    12 ->
                        W.succeedDecode RCase
                            |> W.andMapDecode decodeRExpr
                            |> W.andMapDecode (W.decodeList decodeBranch)

                    13 ->
                        W.succeedDecode (\a b cs -> RLambda { arity = a, body = b, captureSlots = cs })
                            |> W.andMapDecode W.decodeInt
                            |> W.andMapDecode decodeRExpr
                            |> W.andMapDecode (W.decodeList W.decodeInt)

                    14 ->
                        W.succeedDecode RLet
                            |> W.andMapDecode (W.decodeList decodeRLetBinding)
                            |> W.andMapDecode decodeRExpr

                    15 ->
                        W.succeedDecode RApply
                            |> W.andMapDecode decodeRExpr
                            |> W.andMapDecode (W.decodeList decodeRExpr)

                    16 ->
                        decodeRExpr |> mapDecode RNegate

                    17 ->
                        W.decodeList decodeRExpr |> mapDecode RList

                    18 ->
                        W.succeedDecode RTuple2
                            |> W.andMapDecode decodeRExpr
                            |> W.andMapDecode decodeRExpr

                    19 ->
                        W.succeedDecode RTuple3
                            |> W.andMapDecode decodeRExpr
                            |> W.andMapDecode decodeRExpr
                            |> W.andMapDecode decodeRExpr

                    20 ->
                        W.decodeList decodeRecordField |> mapDecode RRecord

                    21 ->
                        W.succeedDecode RRecordAccess
                            |> W.andMapDecode decodeRExpr
                            |> W.andMapDecode W.decodeString

                    22 ->
                        W.succeedDecode RRecordUpdate
                            |> W.andMapDecode W.decodeInt
                            |> W.andMapDecode (W.decodeList decodeRecordField)

                    23 ->
                        W.succeedDecode RGLSL

                    _ ->
                        W.failDecode
            )


encodeBranch : ( RPattern, RExpr ) -> W.Encoder
encodeBranch ( pat, body ) =
    W.encodeSequenceWithoutLength [ encodeRPattern pat, encodeRExpr body ]


decodeBranch : W.Decoder ( RPattern, RExpr )
decodeBranch =
    W.succeedDecode Tuple.pair
        |> W.andMapDecode decodeRPattern
        |> W.andMapDecode decodeRExpr


encodeRecordField : ( String, RExpr ) -> W.Encoder
encodeRecordField ( name, expr ) =
    W.encodeSequenceWithoutLength [ W.encodeString name, encodeRExpr expr ]


decodeRecordField : W.Decoder ( String, RExpr )
decodeRecordField =
    W.succeedDecode Tuple.pair
        |> W.andMapDecode W.decodeString
        |> W.andMapDecode decodeRExpr



-- RLetBinding


encodeRLetBinding : RLetBinding -> W.Encoder
encodeRLetBinding b =
    W.encodeSequenceWithoutLength
        [ encodeRPattern b.pattern
        , W.encodeInt b.arity
        , encodeRExpr b.body
        , W.encodeString b.debugName
        ]


decodeRLetBinding : W.Decoder RLetBinding
decodeRLetBinding =
    W.succeedDecode RLetBinding
        |> W.andMapDecode decodeRPattern
        |> W.andMapDecode W.decodeInt
        |> W.andMapDecode decodeRExpr
        |> W.andMapDecode W.decodeString



-- RPattern


encodeRPattern : RPattern -> W.Encoder
encodeRPattern pat =
    case pat of
        RPVar ->
            W.encodeUnsignedInt8 0

        RPWildcard ->
            W.encodeUnsignedInt8 1

        RPUnit ->
            W.encodeUnsignedInt8 2

        RPInt n ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 3, W.encodeInt n ]

        RPFloat f ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 4, W.encodeFloat64 f ]

        RPChar c ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 5, W.encodeChar c ]

        RPString s ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 6, W.encodeString s ]

        RPTuple2 a b ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 7, encodeRPattern a, encodeRPattern b ]

        RPTuple3 a b c ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 8, encodeRPattern a, encodeRPattern b, encodeRPattern c ]

        RPRecord fields ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 9, W.encodeList W.encodeString fields ]

        RPCons head tail ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 10, encodeRPattern head, encodeRPattern tail ]

        RPList items ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 11, W.encodeList encodeRPattern items ]

        RPCtor ref args ->
            W.encodeSequenceWithoutLength
                [ W.encodeUnsignedInt8 12
                , W.encodeList W.encodeString ref.moduleName
                , W.encodeString ref.name
                , W.encodeList encodeRPattern args
                ]

        RPAs inner ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 13, encodeRPattern inner ]


decodeRPattern : W.Decoder RPattern
decodeRPattern =
    W.decodeUnsignedInt8
        |> W.andThenDecode
            (\tag ->
                case tag of
                    0 ->
                        W.succeedDecode RPVar

                    1 ->
                        W.succeedDecode RPWildcard

                    2 ->
                        W.succeedDecode RPUnit

                    3 ->
                        W.decodeInt |> mapDecode RPInt

                    4 ->
                        W.decodeFloat64 |> mapDecode RPFloat

                    5 ->
                        W.decodeChar |> mapDecode RPChar

                    6 ->
                        W.decodeString |> mapDecode RPString

                    7 ->
                        W.succeedDecode RPTuple2
                            |> W.andMapDecode decodeRPattern
                            |> W.andMapDecode decodeRPattern

                    8 ->
                        W.succeedDecode RPTuple3
                            |> W.andMapDecode decodeRPattern
                            |> W.andMapDecode decodeRPattern
                            |> W.andMapDecode decodeRPattern

                    9 ->
                        W.decodeList W.decodeString |> mapDecode RPRecord

                    10 ->
                        W.succeedDecode RPCons
                            |> W.andMapDecode decodeRPattern
                            |> W.andMapDecode decodeRPattern

                    11 ->
                        W.decodeList decodeRPattern |> mapDecode RPList

                    12 ->
                        W.succeedDecode (\m n args -> RPCtor { moduleName = m, name = n } args)
                            |> W.andMapDecode (W.decodeList W.decodeString)
                            |> W.andMapDecode W.decodeString
                            |> W.andMapDecode (W.decodeList decodeRPattern)

                    13 ->
                        decodeRPattern |> mapDecode RPAs

                    _ ->
                        W.failDecode
            )



-- helpers


mapDecode : (a -> b) -> W.Decoder a -> W.Decoder b
mapDecode f decoder =
    W.succeedDecode f |> W.andMapDecode decoder
