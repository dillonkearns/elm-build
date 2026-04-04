module AstWireCodec exposing (encodeFile, decodeFile, encodeToBytes, decodeFromBytes)

{-| Binary codecs for Elm.Syntax.File using Lamdera.Wire3 primitives.
Much more compact than JSON encoding (~3-4x smaller).
-}

import Bytes exposing (Bytes)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (CaseBlock, Cases, Expression(..), Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation(..))
import Lamdera.Wire3 as W


encodeToBytes : File -> Bytes
encodeToBytes file =
    W.bytesEncode (encodeFile file)


decodeFromBytes : Bytes -> Maybe File
decodeFromBytes bytes =
    W.bytesDecode decodeFile bytes



-- File


encodeFile : File -> W.Encoder
encodeFile { moduleDefinition, imports, declarations, comments } =
    W.encodeSequence
        [ encodeNode encodeModule moduleDefinition
        , W.encodeList (encodeNode encodeImport) imports
        , W.encodeList (encodeNode encodeDeclaration) declarations
        , W.encodeList (encodeNode W.encodeString) comments
        ]


decodeFile : W.Decoder File
decodeFile =
    W.succeedDecode File
        |> W.andMapDecode (decodeNode decodeModule)
        |> W.andMapDecode (W.decodeList (decodeNode decodeImport))
        |> W.andMapDecode (W.decodeList (decodeNode decodeDeclaration))
        |> W.andMapDecode (W.decodeList (decodeNode W.decodeString))



-- Node & Range


encodeNode : (a -> W.Encoder) -> Node a -> W.Encoder
encodeNode enc (Node range value) =
    W.encodeSequence [ encodeRange range, enc value ]


decodeNode : W.Decoder a -> W.Decoder (Node a)
decodeNode dec =
    W.succeedDecode Node |> W.andMapDecode decodeRange |> W.andMapDecode dec


encodeRange : Range -> W.Encoder
encodeRange { start, end } =
    W.encodeSequence
        [ W.encodeInt start.row, W.encodeInt start.column
        , W.encodeInt end.row, W.encodeInt end.column
        ]


decodeRange : W.Decoder Range
decodeRange =
    W.succeedDecode (\sr sc er ec -> { start = { row = sr, column = sc }, end = { row = er, column = ec } })
        |> W.andMapDecode W.decodeInt
        |> W.andMapDecode W.decodeInt
        |> W.andMapDecode W.decodeInt
        |> W.andMapDecode W.decodeInt



-- Module


encodeModule : Module -> W.Encoder
encodeModule mod =
    case mod of
        NormalModule data ->
            W.encodeSequence [ W.encodeInt 0, encodeNode encodeModuleName data.moduleName, encodeNode encodeExposing data.exposingList ]

        PortModule data ->
            W.encodeSequence [ W.encodeInt 1, encodeNode encodeModuleName data.moduleName, encodeNode encodeExposing data.exposingList ]

        EffectModule data ->
            W.encodeSequence [ W.encodeInt 2, encodeNode encodeModuleName data.moduleName, encodeNode encodeExposing data.exposingList ]


decodeModule : W.Decoder Module
decodeModule =
    W.decodeInt
        |> W.andThenDecode
            (\tag ->
                case tag of
                    0 ->
                        W.succeedDecode (\mn el -> NormalModule { moduleName = mn, exposingList = el })
                            |> W.andMapDecode (decodeNode decodeModuleName)
                            |> W.andMapDecode (decodeNode decodeExposing)

                    1 ->
                        W.succeedDecode (\mn el -> PortModule { moduleName = mn, exposingList = el })
                            |> W.andMapDecode (decodeNode decodeModuleName)
                            |> W.andMapDecode (decodeNode decodeExposing)

                    _ ->
                        W.succeedDecode (\mn el -> EffectModule { moduleName = mn, exposingList = el, command = Nothing, subscription = Nothing })
                            |> W.andMapDecode (decodeNode decodeModuleName)
                            |> W.andMapDecode (decodeNode decodeExposing)
            )


encodeModuleName : List String -> W.Encoder
encodeModuleName =
    W.encodeList W.encodeString


decodeModuleName : W.Decoder (List String)
decodeModuleName =
    W.decodeList W.decodeString



-- Exposing


encodeExposing : Exposing -> W.Encoder
encodeExposing exp =
    case exp of
        All _ ->
            W.encodeInt 0

        Explicit list ->
            W.encodeSequence [ W.encodeInt 1, W.encodeList (encodeNode encodeTopLevelExpose) list ]


decodeExposing : W.Decoder Exposing
decodeExposing =
    W.decodeInt
        |> W.andThenDecode
            (\tag ->
                case tag of
                    0 ->
                        W.succeedDecode (All { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } })

                    _ ->
                        W.succeedDecode Explicit |> W.andMapDecode (W.decodeList (decodeNode decodeTopLevelExpose))
            )


encodeTopLevelExpose : TopLevelExpose -> W.Encoder
encodeTopLevelExpose exp =
    case exp of
        InfixExpose s ->
            W.encodeSequence [ W.encodeInt 0, W.encodeString s ]

        FunctionExpose s ->
            W.encodeSequence [ W.encodeInt 1, W.encodeString s ]

        TypeOrAliasExpose s ->
            W.encodeSequence [ W.encodeInt 2, W.encodeString s ]

        TypeExpose { name, open } ->
            W.encodeSequence [ W.encodeInt 3, W.encodeString name, W.encodeMaybe encodeRange open ]


decodeTopLevelExpose : W.Decoder TopLevelExpose
decodeTopLevelExpose =
    W.decodeInt
        |> W.andThenDecode
            (\tag ->
                case tag of
                    0 -> W.succeedDecode InfixExpose |> W.andMapDecode W.decodeString
                    1 -> W.succeedDecode FunctionExpose |> W.andMapDecode W.decodeString
                    2 -> W.succeedDecode TypeOrAliasExpose |> W.andMapDecode W.decodeString
                    _ -> W.succeedDecode (\n o -> TypeExpose { name = n, open = o }) |> W.andMapDecode W.decodeString |> W.andMapDecode (W.decodeMaybe decodeRange)
            )



-- Import


encodeImport : Import -> W.Encoder
encodeImport { moduleName, moduleAlias, exposingList } =
    W.encodeSequence
        [ encodeNode encodeModuleName moduleName
        , W.encodeMaybe (encodeNode encodeModuleName) moduleAlias
        , W.encodeMaybe (encodeNode encodeExposing) exposingList
        ]


decodeImport : W.Decoder Import
decodeImport =
    W.succeedDecode (\mn ma el -> { moduleName = mn, moduleAlias = ma, exposingList = el })
        |> W.andMapDecode (decodeNode decodeModuleName)
        |> W.andMapDecode (W.decodeMaybe (decodeNode decodeModuleName))
        |> W.andMapDecode (W.decodeMaybe (decodeNode decodeExposing))



-- Declaration


encodeDeclaration : Declaration -> W.Encoder
encodeDeclaration decl =
    case decl of
        FunctionDeclaration func ->
            W.encodeSequence
                [ W.encodeInt 0
                , W.encodeMaybe (encodeNode W.encodeString) func.documentation
                , W.encodeMaybe (encodeNode encodeSignature) func.signature
                , encodeNode encodeFunctionImpl func.declaration
                ]

        AliasDeclaration alias_ ->
            W.encodeSequence
                [ W.encodeInt 1
                , W.encodeMaybe (encodeNode W.encodeString) alias_.documentation
                , encodeNode W.encodeString alias_.name
                , W.encodeList (encodeNode W.encodeString) alias_.generics
                , encodeNode encodeTypeAnnotation alias_.typeAnnotation
                ]

        CustomTypeDeclaration type_ ->
            W.encodeSequence
                [ W.encodeInt 2
                , W.encodeMaybe (encodeNode W.encodeString) type_.documentation
                , encodeNode W.encodeString type_.name
                , W.encodeList (encodeNode W.encodeString) type_.generics
                , W.encodeList (encodeNode encodeValueConstructor) type_.constructors
                ]

        PortDeclaration sig ->
            W.encodeSequence [ W.encodeInt 3, encodeNode W.encodeString sig.name, encodeNode encodeTypeAnnotation sig.typeAnnotation ]

        InfixDeclaration inf ->
            W.encodeSequence [ W.encodeInt 4, encodeNode encodeInfixDirection inf.direction, encodeNode W.encodeInt inf.precedence, encodeNode W.encodeString inf.operator, encodeNode W.encodeString inf.function ]

        Destructuring pat expr ->
            W.encodeSequence [ W.encodeInt 5, encodeNode encodePattern pat, encodeNode encodeExpression expr ]


decodeDeclaration : W.Decoder Declaration
decodeDeclaration =
    W.decodeInt
        |> W.andThenDecode
            (\tag ->
                case tag of
                    0 ->
                        W.succeedDecode (\doc sig impl -> FunctionDeclaration { documentation = doc, signature = sig, declaration = impl })
                            |> W.andMapDecode (W.decodeMaybe (decodeNode W.decodeString))
                            |> W.andMapDecode (W.decodeMaybe (decodeNode decodeSignature))
                            |> W.andMapDecode (decodeNode decodeFunctionImpl)

                    1 ->
                        W.succeedDecode (\doc name gens ta -> AliasDeclaration { documentation = doc, name = name, generics = gens, typeAnnotation = ta })
                            |> W.andMapDecode (W.decodeMaybe (decodeNode W.decodeString))
                            |> W.andMapDecode (decodeNode W.decodeString)
                            |> W.andMapDecode (W.decodeList (decodeNode W.decodeString))
                            |> W.andMapDecode (decodeNode decodeTypeAnnotation)

                    2 ->
                        W.succeedDecode (\doc name gens ctors -> CustomTypeDeclaration { documentation = doc, name = name, generics = gens, constructors = ctors })
                            |> W.andMapDecode (W.decodeMaybe (decodeNode W.decodeString))
                            |> W.andMapDecode (decodeNode W.decodeString)
                            |> W.andMapDecode (W.decodeList (decodeNode W.decodeString))
                            |> W.andMapDecode (W.decodeList (decodeNode decodeValueConstructor))

                    3 ->
                        W.succeedDecode (\name ta -> PortDeclaration { name = name, typeAnnotation = ta })
                            |> W.andMapDecode (decodeNode W.decodeString)
                            |> W.andMapDecode (decodeNode decodeTypeAnnotation)

                    4 ->
                        W.succeedDecode (\dir prec op fn -> InfixDeclaration { direction = dir, precedence = prec, operator = op, function = fn })
                            |> W.andMapDecode (decodeNode decodeInfixDirection)
                            |> W.andMapDecode (decodeNode W.decodeInt)
                            |> W.andMapDecode (decodeNode W.decodeString)
                            |> W.andMapDecode (decodeNode W.decodeString)

                    _ ->
                        W.succeedDecode Destructuring
                            |> W.andMapDecode (decodeNode decodePattern)
                            |> W.andMapDecode (decodeNode decodeExpression)
            )


encodeValueConstructor : ValueConstructor -> W.Encoder
encodeValueConstructor { name, arguments } =
    W.encodeSequence [ encodeNode W.encodeString name, W.encodeList (encodeNode encodeTypeAnnotation) arguments ]


decodeValueConstructor : W.Decoder ValueConstructor
decodeValueConstructor =
    W.succeedDecode (\n a -> { name = n, arguments = a })
        |> W.andMapDecode (decodeNode W.decodeString)
        |> W.andMapDecode (W.decodeList (decodeNode decodeTypeAnnotation))


encodeSignature : Signature -> W.Encoder
encodeSignature { name, typeAnnotation } =
    W.encodeSequence [ encodeNode W.encodeString name, encodeNode encodeTypeAnnotation typeAnnotation ]


decodeSignature : W.Decoder Signature
decodeSignature =
    W.succeedDecode (\n ta -> { name = n, typeAnnotation = ta })
        |> W.andMapDecode (decodeNode W.decodeString)
        |> W.andMapDecode (decodeNode decodeTypeAnnotation)


encodeFunctionImpl : Elm.Syntax.Expression.FunctionImplementation -> W.Encoder
encodeFunctionImpl { name, arguments, expression } =
    W.encodeSequence [ encodeNode W.encodeString name, W.encodeList (encodeNode encodePattern) arguments, encodeNode encodeExpression expression ]


decodeFunctionImpl : W.Decoder Elm.Syntax.Expression.FunctionImplementation
decodeFunctionImpl =
    W.succeedDecode (\n a e -> { name = n, arguments = a, expression = e })
        |> W.andMapDecode (decodeNode W.decodeString)
        |> W.andMapDecode (W.decodeList (decodeNode decodePattern))
        |> W.andMapDecode (decodeNode decodeExpression)


encodeInfixDirection : InfixDirection -> W.Encoder
encodeInfixDirection dir =
    case dir of
        Left -> W.encodeInt 0
        Right -> W.encodeInt 1
        Non -> W.encodeInt 2


decodeInfixDirection : W.Decoder InfixDirection
decodeInfixDirection =
    W.decodeInt |> W.andThenDecode (\t -> case t of
        0 -> W.succeedDecode Left
        1 -> W.succeedDecode Right
        _ -> W.succeedDecode Non)



-- TypeAnnotation


encodeTypeAnnotation : TypeAnnotation -> W.Encoder
encodeTypeAnnotation ta =
    case ta of
        GenericType s -> W.encodeSequence [ W.encodeInt 0, W.encodeString s ]
        Typed name args -> W.encodeSequence [ W.encodeInt 1, encodeNode (W.encodePair encodeModuleName W.encodeString) name, W.encodeList (encodeNode encodeTypeAnnotation) args ]
        Unit -> W.encodeInt 2
        Tupled items -> W.encodeSequence [ W.encodeInt 3, W.encodeList (encodeNode encodeTypeAnnotation) items ]
        Record fields -> W.encodeSequence [ W.encodeInt 4, W.encodeList (encodeNode encodeRecordField) fields ]
        GenericRecord name fields -> W.encodeSequence [ W.encodeInt 5, encodeNode W.encodeString name, encodeNode (W.encodeList (encodeNode encodeRecordField)) fields ]
        FunctionTypeAnnotation left right -> W.encodeSequence [ W.encodeInt 6, encodeNode encodeTypeAnnotation left, encodeNode encodeTypeAnnotation right ]


decodeTypeAnnotation : W.Decoder TypeAnnotation
decodeTypeAnnotation =
    W.decodeInt
        |> W.andThenDecode
            (\tag ->
                case tag of
                    0 -> W.succeedDecode GenericType |> W.andMapDecode W.decodeString
                    1 -> W.succeedDecode Typed |> W.andMapDecode (decodeNode (W.decodePair decodeModuleName W.decodeString)) |> W.andMapDecode (W.decodeList (decodeNode decodeTypeAnnotation))
                    2 -> W.succeedDecode Unit
                    3 -> W.succeedDecode Tupled |> W.andMapDecode (W.decodeList (decodeNode decodeTypeAnnotation))
                    4 -> W.succeedDecode Record |> W.andMapDecode (W.decodeList (decodeNode decodeRecordField))
                    5 -> W.succeedDecode GenericRecord |> W.andMapDecode (decodeNode W.decodeString) |> W.andMapDecode (decodeNode (W.decodeList (decodeNode decodeRecordField)))
                    _ -> W.succeedDecode FunctionTypeAnnotation |> W.andMapDecode (decodeNode decodeTypeAnnotation) |> W.andMapDecode (decodeNode decodeTypeAnnotation)
            )


encodeRecordField : RecordField -> W.Encoder
encodeRecordField ( name, typeAnnotation ) =
    W.encodeSequence [ encodeNode W.encodeString name, encodeNode encodeTypeAnnotation typeAnnotation ]


decodeRecordField : W.Decoder RecordField
decodeRecordField =
    W.succeedDecode Tuple.pair
        |> W.andMapDecode (decodeNode W.decodeString)
        |> W.andMapDecode (decodeNode decodeTypeAnnotation)



-- Expression


encodeExpression : Expression -> W.Encoder
encodeExpression expr =
    case expr of
        UnitExpr -> W.encodeInt 0
        Application nodes -> W.encodeSequence [ W.encodeInt 1, W.encodeList (encodeNode encodeExpression) nodes ]
        OperatorApplication op dir left right -> W.encodeSequence [ W.encodeInt 2, W.encodeString op, encodeInfixDirection dir, encodeNode encodeExpression left, encodeNode encodeExpression right ]
        FunctionOrValue mod name -> W.encodeSequence [ W.encodeInt 3, encodeModuleName mod, W.encodeString name ]
        IfBlock c t e -> W.encodeSequence [ W.encodeInt 4, encodeNode encodeExpression c, encodeNode encodeExpression t, encodeNode encodeExpression e ]
        PrefixOperator s -> W.encodeSequence [ W.encodeInt 5, W.encodeString s ]
        Operator s -> W.encodeSequence [ W.encodeInt 6, W.encodeString s ]
        Integer i -> W.encodeSequence [ W.encodeInt 7, W.encodeInt i ]
        Hex i -> W.encodeSequence [ W.encodeInt 8, W.encodeInt i ]
        Floatable f -> W.encodeSequence [ W.encodeInt 9, W.encodeFloat f ]
        Negation e -> W.encodeSequence [ W.encodeInt 10, encodeNode encodeExpression e ]
        Literal s -> W.encodeSequence [ W.encodeInt 11, W.encodeString s ]
        CharLiteral c -> W.encodeSequence [ W.encodeInt 12, W.encodeChar c ]
        TupledExpression es -> W.encodeSequence [ W.encodeInt 13, W.encodeList (encodeNode encodeExpression) es ]
        ParenthesizedExpression e -> W.encodeSequence [ W.encodeInt 14, encodeNode encodeExpression e ]
        LetExpression lb -> W.encodeSequence [ W.encodeInt 15, encodeLetBlock lb ]
        CaseExpression cb -> W.encodeSequence [ W.encodeInt 16, encodeCaseBlock cb ]
        LambdaExpression lam -> W.encodeSequence [ W.encodeInt 17, encodeLambda lam ]
        RecordExpr fields -> W.encodeSequence [ W.encodeInt 18, W.encodeList (encodeNode encodeRecordSetter) fields ]
        ListExpr es -> W.encodeSequence [ W.encodeInt 19, W.encodeList (encodeNode encodeExpression) es ]
        RecordAccess e field -> W.encodeSequence [ W.encodeInt 20, encodeNode encodeExpression e, encodeNode W.encodeString field ]
        RecordAccessFunction s -> W.encodeSequence [ W.encodeInt 21, W.encodeString s ]
        RecordUpdateExpression name setters -> W.encodeSequence [ W.encodeInt 22, encodeNode W.encodeString name, W.encodeList (encodeNode encodeRecordSetter) setters ]
        GLSLExpression s -> W.encodeSequence [ W.encodeInt 23, W.encodeString s ]


decodeExpression : W.Decoder Expression
decodeExpression =
    W.decodeInt
        |> W.andThenDecode
            (\tag ->
                case tag of
                    0 -> W.succeedDecode UnitExpr
                    1 -> W.succeedDecode Application |> W.andMapDecode (W.decodeList (decodeNode decodeExpression))
                    2 -> W.succeedDecode OperatorApplication |> W.andMapDecode W.decodeString |> W.andMapDecode decodeInfixDirection |> W.andMapDecode (decodeNode decodeExpression) |> W.andMapDecode (decodeNode decodeExpression)
                    3 -> W.succeedDecode FunctionOrValue |> W.andMapDecode decodeModuleName |> W.andMapDecode W.decodeString
                    4 -> W.succeedDecode IfBlock |> W.andMapDecode (decodeNode decodeExpression) |> W.andMapDecode (decodeNode decodeExpression) |> W.andMapDecode (decodeNode decodeExpression)
                    5 -> W.succeedDecode PrefixOperator |> W.andMapDecode W.decodeString
                    6 -> W.succeedDecode Operator |> W.andMapDecode W.decodeString
                    7 -> W.succeedDecode Integer |> W.andMapDecode W.decodeInt
                    8 -> W.succeedDecode Hex |> W.andMapDecode W.decodeInt
                    9 -> W.succeedDecode Floatable |> W.andMapDecode W.decodeFloat
                    10 -> W.succeedDecode Negation |> W.andMapDecode (decodeNode decodeExpression)
                    11 -> W.succeedDecode Literal |> W.andMapDecode W.decodeString
                    12 -> W.succeedDecode CharLiteral |> W.andMapDecode W.decodeChar
                    13 -> W.succeedDecode TupledExpression |> W.andMapDecode (W.decodeList (decodeNode decodeExpression))
                    14 -> W.succeedDecode ParenthesizedExpression |> W.andMapDecode (decodeNode decodeExpression)
                    15 -> W.succeedDecode LetExpression |> W.andMapDecode decodeLetBlock
                    16 -> W.succeedDecode CaseExpression |> W.andMapDecode decodeCaseBlock
                    17 -> W.succeedDecode LambdaExpression |> W.andMapDecode decodeLambda
                    18 -> W.succeedDecode RecordExpr |> W.andMapDecode (W.decodeList (decodeNode decodeRecordSetter))
                    19 -> W.succeedDecode ListExpr |> W.andMapDecode (W.decodeList (decodeNode decodeExpression))
                    20 -> W.succeedDecode RecordAccess |> W.andMapDecode (decodeNode decodeExpression) |> W.andMapDecode (decodeNode W.decodeString)
                    21 -> W.succeedDecode RecordAccessFunction |> W.andMapDecode W.decodeString
                    22 -> W.succeedDecode RecordUpdateExpression |> W.andMapDecode (decodeNode W.decodeString) |> W.andMapDecode (W.decodeList (decodeNode decodeRecordSetter))
                    _ -> W.succeedDecode GLSLExpression |> W.andMapDecode W.decodeString
            )


encodeRecordSetter : RecordSetter -> W.Encoder
encodeRecordSetter ( name, expr ) =
    W.encodeSequence [ encodeNode W.encodeString name, encodeNode encodeExpression expr ]


decodeRecordSetter : W.Decoder RecordSetter
decodeRecordSetter =
    W.succeedDecode Tuple.pair |> W.andMapDecode (decodeNode W.decodeString) |> W.andMapDecode (decodeNode decodeExpression)


encodeLetBlock : LetBlock -> W.Encoder
encodeLetBlock { declarations, expression } =
    W.encodeSequence [ W.encodeList (encodeNode encodeLetDeclaration) declarations, encodeNode encodeExpression expression ]


decodeLetBlock : W.Decoder LetBlock
decodeLetBlock =
    W.succeedDecode (\d e -> { declarations = d, expression = e })
        |> W.andMapDecode (W.decodeList (decodeNode decodeLetDeclaration))
        |> W.andMapDecode (decodeNode decodeExpression)


encodeLetDeclaration : LetDeclaration -> W.Encoder
encodeLetDeclaration decl =
    case decl of
        LetFunction func ->
            W.encodeSequence [ W.encodeInt 0, W.encodeMaybe (encodeNode W.encodeString) func.documentation, W.encodeMaybe (encodeNode encodeSignature) func.signature, encodeNode encodeFunctionImpl func.declaration ]

        LetDestructuring pat expr ->
            W.encodeSequence [ W.encodeInt 1, encodeNode encodePattern pat, encodeNode encodeExpression expr ]


decodeLetDeclaration : W.Decoder LetDeclaration
decodeLetDeclaration =
    W.decodeInt
        |> W.andThenDecode
            (\tag ->
                case tag of
                    0 ->
                        W.succeedDecode (\doc sig impl -> LetFunction { documentation = doc, signature = sig, declaration = impl })
                            |> W.andMapDecode (W.decodeMaybe (decodeNode W.decodeString))
                            |> W.andMapDecode (W.decodeMaybe (decodeNode decodeSignature))
                            |> W.andMapDecode (decodeNode decodeFunctionImpl)

                    _ ->
                        W.succeedDecode LetDestructuring |> W.andMapDecode (decodeNode decodePattern) |> W.andMapDecode (decodeNode decodeExpression)
            )


encodeCaseBlock : CaseBlock -> W.Encoder
encodeCaseBlock { expression, cases } =
    W.encodeSequence [ encodeNode encodeExpression expression, W.encodeList encodeCase cases ]


decodeCaseBlock : W.Decoder CaseBlock
decodeCaseBlock =
    W.succeedDecode (\e c -> { expression = e, cases = c })
        |> W.andMapDecode (decodeNode decodeExpression)
        |> W.andMapDecode (W.decodeList decodeCase)


encodeCase : ( Node Pattern, Node Expression ) -> W.Encoder
encodeCase ( pat, expr ) =
    W.encodeSequence [ encodeNode encodePattern pat, encodeNode encodeExpression expr ]


decodeCase : W.Decoder ( Node Pattern, Node Expression )
decodeCase =
    W.succeedDecode Tuple.pair |> W.andMapDecode (decodeNode decodePattern) |> W.andMapDecode (decodeNode decodeExpression)


encodeLambda : Lambda -> W.Encoder
encodeLambda { args, expression } =
    W.encodeSequence [ W.encodeList (encodeNode encodePattern) args, encodeNode encodeExpression expression ]


decodeLambda : W.Decoder Lambda
decodeLambda =
    W.succeedDecode (\a e -> { args = a, expression = e })
        |> W.andMapDecode (W.decodeList (decodeNode decodePattern))
        |> W.andMapDecode (decodeNode decodeExpression)



-- Pattern


encodePattern : Pattern -> W.Encoder
encodePattern pat =
    case pat of
        AllPattern -> W.encodeInt 0
        UnitPattern -> W.encodeInt 1
        CharPattern c -> W.encodeSequence [ W.encodeInt 2, W.encodeChar c ]
        StringPattern s -> W.encodeSequence [ W.encodeInt 3, W.encodeString s ]
        IntPattern i -> W.encodeSequence [ W.encodeInt 4, W.encodeInt i ]
        HexPattern i -> W.encodeSequence [ W.encodeInt 5, W.encodeInt i ]
        FloatPattern f -> W.encodeSequence [ W.encodeInt 6, W.encodeFloat f ]
        TuplePattern ps -> W.encodeSequence [ W.encodeInt 7, W.encodeList (encodeNode encodePattern) ps ]
        RecordPattern fields -> W.encodeSequence [ W.encodeInt 8, W.encodeList (encodeNode W.encodeString) fields ]
        UnConsPattern h t -> W.encodeSequence [ W.encodeInt 9, encodeNode encodePattern h, encodeNode encodePattern t ]
        ListPattern ps -> W.encodeSequence [ W.encodeInt 10, W.encodeList (encodeNode encodePattern) ps ]
        VarPattern s -> W.encodeSequence [ W.encodeInt 11, W.encodeString s ]
        NamedPattern { moduleName, name } args -> W.encodeSequence [ W.encodeInt 12, encodeModuleName moduleName, W.encodeString name, W.encodeList (encodeNode encodePattern) args ]
        AsPattern p alias_ -> W.encodeSequence [ W.encodeInt 13, encodeNode encodePattern p, encodeNode W.encodeString alias_ ]
        ParenthesizedPattern p -> W.encodeSequence [ W.encodeInt 14, encodeNode encodePattern p ]


decodePattern : W.Decoder Pattern
decodePattern =
    W.decodeInt
        |> W.andThenDecode
            (\tag ->
                case tag of
                    0 -> W.succeedDecode AllPattern
                    1 -> W.succeedDecode UnitPattern
                    2 -> W.succeedDecode CharPattern |> W.andMapDecode W.decodeChar
                    3 -> W.succeedDecode StringPattern |> W.andMapDecode W.decodeString
                    4 -> W.succeedDecode IntPattern |> W.andMapDecode W.decodeInt
                    5 -> W.succeedDecode HexPattern |> W.andMapDecode W.decodeInt
                    6 -> W.succeedDecode FloatPattern |> W.andMapDecode W.decodeFloat
                    7 -> W.succeedDecode TuplePattern |> W.andMapDecode (W.decodeList (decodeNode decodePattern))
                    8 -> W.succeedDecode RecordPattern |> W.andMapDecode (W.decodeList (decodeNode W.decodeString))
                    9 -> W.succeedDecode UnConsPattern |> W.andMapDecode (decodeNode decodePattern) |> W.andMapDecode (decodeNode decodePattern)
                    10 -> W.succeedDecode ListPattern |> W.andMapDecode (W.decodeList (decodeNode decodePattern))
                    11 -> W.succeedDecode VarPattern |> W.andMapDecode W.decodeString
                    12 -> W.succeedDecode (\m n a -> NamedPattern { moduleName = m, name = n } a) |> W.andMapDecode decodeModuleName |> W.andMapDecode W.decodeString |> W.andMapDecode (W.decodeList (decodeNode decodePattern))
                    13 -> W.succeedDecode AsPattern |> W.andMapDecode (decodeNode decodePattern) |> W.andMapDecode (decodeNode W.decodeString)
                    _ -> W.succeedDecode ParenthesizedPattern |> W.andMapDecode (decodeNode decodePattern)
            )
