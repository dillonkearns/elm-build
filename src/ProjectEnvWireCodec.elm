module ProjectEnvWireCodec exposing
    ( decodeWireFields
    , encodeWireFields
    , roundTrip
    )

{-| Wire3 codec for `Eval.Module.WireFields` — the wire-shipping
shape of a `ProjectEnv`. Pairs with `Eval.Module.toWireFields` /
`fromWireFields` on the elm-interpreter side: the worker pool's
main→worker handoff goes through

    main: env |> toWireFields |> encodeWireFields |> ship
    worker: receive |> decodeWireFields |> Maybe.map fromWireFields

Reuses existing piece codecs:

  - `RExprWireCodec.encodeRExpr` / `decodeRExpr` — for the resolved IR
  - `ValueWireCodec.encodeValue` / `decodeValue` — for Values (caller
    must filter precomputedValues through `Eval.Module.isLosslessValue`
    before encoding)
  - `InterpreterProject.encodeFunctionImplementationNoRanges` / decode —
    for parsed function bodies (Range-stripped per step-8a finding)
  - `InterpreterProject.encodeExposed` / `decodeExposed` — for module
    interfaces

What this module adds:

  - `ImportedNames` codec
  - `ResolveError` / `ResolveErrorEntry` codec
  - Glue for the WireFields record itself

-}

import Bytes exposing (Bytes)
import Dict
import Elm.Interface exposing (Exposed)
import Elm.Syntax.Expression exposing (FunctionImplementation)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Eval.Module
import Eval.ResolvedIR as IR
import Eval.Resolver as Resolver
import FastDict
import InterpreterProject
import Lamdera.Wire3 as W
import RExprWireCodec
import Types exposing (ImportedNames, Value)
import ValueWireCodec



-- TOP-LEVEL


encodeWireFields : Eval.Module.WireFields -> Bytes
encodeWireFields w =
    W.bytesEncode <|
        W.encodeSequenceWithoutLength
            [ encodeStringDict (encodeStringDict encodeFunctionImpl) w.sharedFunctions
            , encodeStringDict encodeImportedNames w.sharedModuleImports
            , encodeStringDict (encodeStringDict ValueWireCodec.encodeValue) w.sharedPrecomputedValues
            , encodeElmDict encodeModuleName (W.encodeList InterpreterProject.encodeExposed) w.allInterfaces
            , encodeFastDict encodeModuleNameNamePair W.encodeInt w.resolvedGlobalIds
            , encodeFastDict W.encodeInt RExprWireCodec.encodeRExpr w.resolvedBodies
            , encodeFastDict W.encodeInt encodeModuleNameNamePair w.resolvedGlobalIdToName
            , encodeFastDict W.encodeInt ValueWireCodec.encodeValue w.resolvedGlobals
            , W.encodeList encodeResolveErrorEntry w.resolvedErrors
            , encodeModuleName w.currentModule
            , W.encodeString w.currentModuleKey
            ]


decodeWireFields : Bytes -> Maybe Eval.Module.WireFields
decodeWireFields bytes =
    W.bytesDecode wireFieldsDecoder bytes


wireFieldsDecoder : W.Decoder Eval.Module.WireFields
wireFieldsDecoder =
    W.succeedDecode Eval.Module.WireFields
        |> W.andMapDecode (decodeStringDict (decodeStringDict decodeFunctionImpl))
        |> W.andMapDecode (decodeStringDict decodeImportedNames)
        |> W.andMapDecode (decodeStringDict (decodeStringDict ValueWireCodec.decodeValue))
        |> W.andMapDecode (decodeElmDict decodeModuleName (W.decodeList InterpreterProject.decodeExposed))
        |> W.andMapDecode (decodeFastDict decodeModuleNameNamePair W.decodeInt)
        |> W.andMapDecode (decodeFastDict W.decodeInt RExprWireCodec.decodeRExpr)
        |> W.andMapDecode (decodeFastDict W.decodeInt decodeModuleNameNamePair)
        |> W.andMapDecode (decodeFastDict W.decodeInt ValueWireCodec.decodeValue)
        |> W.andMapDecode (W.decodeList decodeResolveErrorEntry)
        |> W.andMapDecode decodeModuleName
        |> W.andMapDecode W.decodeString


roundTrip : Eval.Module.WireFields -> Maybe Eval.Module.WireFields
roundTrip w =
    encodeWireFields w |> decodeWireFields



-- ImportedNames


encodeImportedNames : ImportedNames -> W.Encoder
encodeImportedNames imports =
    W.encodeSequenceWithoutLength
        [ encodeStringDict encodeModuleNameNamePair imports.aliases
        , encodeStringDict encodeModuleNameNamePair imports.exposedValues
        , encodeStringDict encodeModuleNameNamePair imports.exposedConstructors
        ]


decodeImportedNames : W.Decoder ImportedNames
decodeImportedNames =
    W.succeedDecode
        (\aliases vals ctors ->
            { aliases = aliases
            , exposedValues = vals
            , exposedConstructors = ctors
            }
        )
        |> W.andMapDecode (decodeStringDict decodeModuleNameNamePair)
        |> W.andMapDecode (decodeStringDict decodeModuleNameNamePair)
        |> W.andMapDecode (decodeStringDict decodeModuleNameNamePair)



-- ResolveErrorEntry / ResolveError


encodeResolveErrorEntry : Eval.Module.ResolveErrorEntry -> W.Encoder
encodeResolveErrorEntry entry =
    W.encodeSequenceWithoutLength
        [ encodeModuleName entry.moduleName
        , W.encodeString entry.name
        , encodeResolveError entry.error
        ]


decodeResolveErrorEntry : W.Decoder Eval.Module.ResolveErrorEntry
decodeResolveErrorEntry =
    W.succeedDecode
        (\moduleName name error ->
            { moduleName = moduleName
            , name = name
            , error = error
            }
        )
        |> W.andMapDecode decodeModuleName
        |> W.andMapDecode W.decodeString
        |> W.andMapDecode decodeResolveError


encodeResolveError : Resolver.ResolveError -> W.Encoder
encodeResolveError err =
    case err of
        Resolver.UnknownName ref ->
            W.encodeSequenceWithoutLength
                [ W.encodeUnsignedInt8 0
                , W.encodeList W.encodeString ref.moduleName
                , W.encodeString ref.name
                ]

        Resolver.UnknownOperator op ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 1, W.encodeString op ]

        Resolver.UnsupportedExpression msg ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 2, W.encodeString msg ]

        Resolver.InvalidRecordUpdateTarget msg ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 3, W.encodeString msg ]

        Resolver.UnexpectedTupleArity n ->
            W.encodeSequenceWithoutLength [ W.encodeUnsignedInt8 4, W.encodeInt n ]


decodeResolveError : W.Decoder Resolver.ResolveError
decodeResolveError =
    W.decodeUnsignedInt8
        |> W.andThenDecode
            (\tag ->
                case tag of
                    0 ->
                        W.succeedDecode (\m n -> Resolver.UnknownName { moduleName = m, name = n })
                            |> W.andMapDecode (W.decodeList W.decodeString)
                            |> W.andMapDecode W.decodeString

                    1 ->
                        W.decodeString |> mapDecode Resolver.UnknownOperator

                    2 ->
                        W.decodeString |> mapDecode Resolver.UnsupportedExpression

                    3 ->
                        W.decodeString |> mapDecode Resolver.InvalidRecordUpdateTarget

                    4 ->
                        W.decodeInt |> mapDecode Resolver.UnexpectedTupleArity

                    _ ->
                        W.failDecode
            )



-- Wrapper aliases for cross-codec consistency


encodeModuleName : ModuleName -> W.Encoder
encodeModuleName =
    InterpreterProject.encodeModuleName


decodeModuleName : W.Decoder ModuleName
decodeModuleName =
    InterpreterProject.decodeModuleName


encodeModuleNameNamePair : ( ModuleName, String ) -> W.Encoder
encodeModuleNameNamePair ( mn, n ) =
    W.encodeSequenceWithoutLength [ encodeModuleName mn, W.encodeString n ]


decodeModuleNameNamePair : W.Decoder ( ModuleName, String )
decodeModuleNameNamePair =
    W.succeedDecode Tuple.pair
        |> W.andMapDecode decodeModuleName
        |> W.andMapDecode W.decodeString


encodeFunctionImpl : FunctionImplementation -> W.Encoder
encodeFunctionImpl =
    InterpreterProject.encodeFunctionImplementationNoRanges


decodeFunctionImpl : W.Decoder FunctionImplementation
decodeFunctionImpl =
    InterpreterProject.decodeFunctionImplementationNoRanges



-- Generic Dict / FastDict / ElmDict helpers


encodeStringDict : (v -> W.Encoder) -> FastDict.Dict String v -> W.Encoder
encodeStringDict encodeV dict =
    dict
        |> FastDict.toList
        |> W.encodeList
            (\( k, v ) ->
                W.encodeSequenceWithoutLength [ W.encodeString k, encodeV v ]
            )


decodeStringDict : W.Decoder v -> W.Decoder (FastDict.Dict String v)
decodeStringDict decodeV =
    W.decodeList
        (W.succeedDecode Tuple.pair
            |> W.andMapDecode W.decodeString
            |> W.andMapDecode decodeV
        )
        |> mapDecode FastDict.fromList


encodeFastDict : (k -> W.Encoder) -> (v -> W.Encoder) -> FastDict.Dict k v -> W.Encoder
encodeFastDict encodeK encodeV dict =
    dict
        |> FastDict.toList
        |> W.encodeList
            (\( k, v ) ->
                W.encodeSequenceWithoutLength [ encodeK k, encodeV v ]
            )


decodeFastDict : W.Decoder comparable -> W.Decoder v -> W.Decoder (FastDict.Dict comparable v)
decodeFastDict decodeK decodeV =
    W.decodeList
        (W.succeedDecode Tuple.pair
            |> W.andMapDecode decodeK
            |> W.andMapDecode decodeV
        )
        |> mapDecode FastDict.fromList


encodeElmDict : (k -> W.Encoder) -> (v -> W.Encoder) -> Dict.Dict k v -> W.Encoder
encodeElmDict encodeK encodeV dict =
    dict
        |> Dict.toList
        |> W.encodeList
            (\( k, v ) ->
                W.encodeSequenceWithoutLength [ encodeK k, encodeV v ]
            )


decodeElmDict : W.Decoder comparable -> W.Decoder v -> W.Decoder (Dict.Dict comparable v)
decodeElmDict decodeK decodeV =
    W.decodeList
        (W.succeedDecode Tuple.pair
            |> W.andMapDecode decodeK
            |> W.andMapDecode decodeV
        )
        |> mapDecode Dict.fromList



-- Helpers


mapDecode : (a -> b) -> W.Decoder a -> W.Decoder b
mapDecode f decoder =
    W.succeedDecode f |> W.andMapDecode decoder
