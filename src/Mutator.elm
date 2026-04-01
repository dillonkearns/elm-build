module Mutator exposing (Mutation, generateMutations, generateMutationsFromFile, writeFile)

{-| Mutation generator for Elm source code.

Uses elm-syntax to parse source into an AST, then generates mutations
as AST-to-AST transforms. Each mutation produces a modified File AST
that can be fed directly to the interpreter without re-parsing.

-}

import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation, LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Writer


type alias Mutation =
    { line : Int
    , column : Int
    , operator : String
    , description : String
    , mutatedFile : File
    }


{-| Parse source and generate all mutations.
-}
generateMutations : String -> List Mutation
generateMutations source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            generateMutationsFromFile file

        Err _ ->
            []


{-| Generate all mutations from an already-parsed File AST.
-}
generateMutationsFromFile : File -> List Mutation
generateMutationsFromFile file =
    file.declarations
        |> List.indexedMap
            (\declIndex (Node declRange decl) ->
                findMutationsInDeclaration file declIndex decl
            )
        |> List.concat


{-| Write a File AST back to source string (for display/debugging).
-}
writeFile : File -> String
writeFile file =
    Elm.Writer.write (Elm.Writer.writeFile file)


findMutationsInDeclaration : File -> Int -> Declaration -> List Mutation
findMutationsInDeclaration file declIndex decl =
    case decl of
        FunctionDeclaration function ->
            let
                (Node _ impl) =
                    function.declaration
            in
            findMutationsInExpression file declIndex [] impl.expression

        _ ->
            []


{-| Walk the expression AST and collect mutations.

Each mutation stores the full modified File AST — the original file with
one expression node replaced.

-}
findMutationsInExpression : File -> Int -> List Int -> Node Expression -> List Mutation
findMutationsInExpression file declIndex path (Node range expr) =
    let
        recurse : List (Node Expression) -> List Mutation
        recurse nodes =
            nodes
                |> List.indexedMap
                    (\i node ->
                        findMutationsInExpression file declIndex (path ++ [ i ]) node
                    )
                |> List.concat

        childMutations : List Mutation
        childMutations =
            case expr of
                IfBlock cond thenExpr elseExpr ->
                    recurse [ cond, thenExpr, elseExpr ]

                OperatorApplication _ _ left right ->
                    recurse [ left, right ]

                Application nodes ->
                    recurse nodes

                ParenthesizedExpression inner ->
                    recurse [ inner ]

                TupledExpression nodes ->
                    recurse nodes

                ListExpr nodes ->
                    recurse nodes

                LetExpression { declarations, expression } ->
                    let
                        declExprs =
                            declarations
                                |> List.concatMap
                                    (\(Node _ letDecl) ->
                                        case letDecl of
                                            LetFunction f ->
                                                let
                                                    (Node _ impl) =
                                                        f.declaration
                                                in
                                                [ impl.expression ]

                                            LetDestructuring _ e ->
                                                [ e ]
                                    )
                    in
                    recurse (expression :: declExprs)

                CaseExpression { expression, cases } ->
                    recurse (expression :: List.map Tuple.second cases)

                LambdaExpression { expression } ->
                    recurse [ expression ]

                Negation inner ->
                    recurse [ inner ]

                RecordExpr setters ->
                    recurse (List.map (Node.value >> Tuple.second) setters)

                RecordUpdateExpression _ setters ->
                    recurse (List.map (Node.value >> Tuple.second) setters)

                RecordAccess inner _ ->
                    recurse [ inner ]

                _ ->
                    []

        -- Create a mutation by replacing the current expression with a new one
        mutate : String -> String -> Expression -> Mutation
        mutate operator description newExpr =
            { line = range.start.row
            , column = range.start.column
            , operator = operator
            , description = description
            , mutatedFile = replaceExpression file declIndex path (Node range newExpr)
            }

        thisMutation : List Mutation
        thisMutation =
            case expr of
                IfBlock (Node condRange condExpr) (Node thenRange thenExpr) (Node elseRange elseExpr) ->
                    let
                        wrappedCond =
                            case condExpr of
                                FunctionOrValue _ _ ->
                                    -- Simple variable: not x (no parens needed)
                                    Node condRange condExpr

                                _ ->
                                    -- Complex expression: not (expr)
                                    Node condRange (ParenthesizedExpression (Node condRange condExpr))
                    in
                    [ -- negateCondition
                      mutate "negateCondition"
                        "Negated if-condition"
                        (IfBlock
                            (Node condRange
                                (Application
                                    [ Node Range.empty (FunctionOrValue [] "not")
                                    , wrappedCond
                                    ]
                                )
                            )
                            (Node thenRange thenExpr)
                            (Node elseRange elseExpr)
                        )

                    -- dropElseBranch
                    , mutate "dropElseBranch"
                        "Replaced else branch with then branch"
                        (IfBlock (Node condRange condExpr) (Node thenRange thenExpr) (Node elseRange thenExpr))

                    -- conditionalTrue
                    , mutate "conditionalTrue"
                        "Replaced condition with `True`"
                        (IfBlock (Node condRange (FunctionOrValue [] "True")) (Node thenRange thenExpr) (Node elseRange elseExpr))

                    -- conditionalFalse
                    , mutate "conditionalFalse"
                        "Replaced condition with `False`"
                        (IfBlock (Node condRange (FunctionOrValue [] "False")) (Node thenRange thenExpr) (Node elseRange elseExpr))
                    ]

                OperatorApplication op dir left right ->
                    let
                        swapOp newOp opName =
                            mutate opName
                                ("Changed `" ++ op ++ "` to `" ++ newOp ++ "`")
                                (OperatorApplication newOp dir left right)
                    in
                    -- comparison swaps
                    (comparisonSwaps op |> List.map (\newOp -> swapOp newOp "swapComparison"))
                        ++ (comparisonNegations op |> List.map (\newOp -> swapOp newOp "negateComparison"))
                        ++ (arithmeticSwaps op |> List.map (\newOp -> swapOp newOp "replaceArithmetic"))
                        ++ (logicalSwaps op |> List.map (\newOp -> swapOp newOp "swapLogicalOperator"))
                        ++ concatRemovalAst op dir left right range file declIndex path

                FunctionOrValue [] "True" ->
                    [ mutate "swapBooleanLiteral" "Changed `True` to `False`" (FunctionOrValue [] "False") ]

                FunctionOrValue [] "False" ->
                    [ mutate "swapBooleanLiteral" "Changed `False` to `True`" (FunctionOrValue [] "True") ]

                Integer n ->
                    [ mutate "replaceIntLiteral"
                        ("Changed `" ++ String.fromInt n ++ "` to `" ++ String.fromInt (n + 1) ++ "`")
                        (Integer (n + 1))
                    ]

                Literal s ->
                    [ if String.isEmpty s then
                        mutate "replaceStringLiteral" "Changed `\"\"` to `\"mutated!\"`" (Literal "mutated!")

                      else
                        mutate "replaceStringLiteral" ("Changed `\"" ++ s ++ "\"` to `\"\"`") (Literal "")
                    ]

                Application ((Node fnRange (FunctionOrValue fnMod fnName)) :: args) ->
                    functionSwapMutations fnMod fnName fnRange args range file declIndex path
                        ++ (case ( fnMod, fnName, args ) of
                                ( [], "not", [ _ ] ) ->
                                    [ mutate "removeNot" "Removed `not` call" (Application args |> unwrapSingleApp) ]

                                ( [], "Just", _ ) ->
                                    [ mutate "replaceWithNothing" "Replaced `Just ...` with `Nothing`" (FunctionOrValue [] "Nothing") ]

                                ( [ "Maybe" ], "Just", _ ) ->
                                    [ mutate "replaceWithNothing" "Replaced `Just ...` with `Nothing`" (FunctionOrValue [] "Nothing") ]

                                _ ->
                                    []
                           )

                Negation (Node innerRange innerExpr) ->
                    [ mutate "removeNegation" "Removed unary negation" innerExpr ]

                ListExpr elements ->
                    if List.length elements >= 2 then
                        removeListElementMutations elements range file declIndex path
                            ++ [ mutate "emptyList" "Replaced list with `[]`" (ListExpr []) ]

                    else
                        []

                _ ->
                    []
    in
    thisMutation ++ childMutations



-- AST helpers


{-| Replace an expression at a specific path within a file's declaration.
-}
replaceExpression : File -> Int -> List Int -> Node Expression -> File
replaceExpression file declIndex path newExpr =
    { file
        | declarations =
            file.declarations
                |> List.indexedMap
                    (\i decl ->
                        if i == declIndex then
                            replaceExprInDeclaration decl path newExpr

                        else
                            decl
                    )
    }


replaceExprInDeclaration : Node Declaration -> List Int -> Node Expression -> Node Declaration
replaceExprInDeclaration (Node declRange decl) path newExpr =
    Node declRange
        (case decl of
            FunctionDeclaration function ->
                let
                    (Node implRange impl) =
                        function.declaration
                in
                FunctionDeclaration
                    { function
                        | declaration =
                            Node implRange
                                { impl
                                    | expression = replaceExprAtPath impl.expression path newExpr
                                }
                    }

            other ->
                other
        )


replaceExprAtPath : Node Expression -> List Int -> Node Expression -> Node Expression
replaceExprAtPath (Node range expr) path newExpr =
    case path of
        [] ->
            newExpr

        childIndex :: restPath ->
            Node range
                (case expr of
                    IfBlock cond thenE elseE ->
                        case childIndex of
                            0 ->
                                IfBlock (replaceExprAtPath cond restPath newExpr) thenE elseE

                            1 ->
                                IfBlock cond (replaceExprAtPath thenE restPath newExpr) elseE

                            _ ->
                                IfBlock cond thenE (replaceExprAtPath elseE restPath newExpr)

                    OperatorApplication op dir left right ->
                        case childIndex of
                            0 ->
                                OperatorApplication op dir (replaceExprAtPath left restPath newExpr) right

                            _ ->
                                OperatorApplication op dir left (replaceExprAtPath right restPath newExpr)

                    Application nodes ->
                        Application (replaceNth childIndex (replaceExprAtPath) nodes restPath newExpr)

                    ParenthesizedExpression inner ->
                        ParenthesizedExpression (replaceExprAtPath inner restPath newExpr)

                    TupledExpression nodes ->
                        TupledExpression (replaceNth childIndex (replaceExprAtPath) nodes restPath newExpr)

                    ListExpr nodes ->
                        ListExpr (replaceNth childIndex (replaceExprAtPath) nodes restPath newExpr)

                    LetExpression block ->
                        if childIndex == 0 then
                            LetExpression { block | expression = replaceExprAtPath block.expression restPath newExpr }

                        else
                            -- Let declaration expressions — complex, skip for now
                            expr

                    CaseExpression block ->
                        if childIndex == 0 then
                            CaseExpression { block | expression = replaceExprAtPath block.expression restPath newExpr }

                        else
                            let
                                caseIndex =
                                    childIndex - 1
                            in
                            CaseExpression
                                { block
                                    | cases =
                                        block.cases
                                            |> List.indexedMap
                                                (\i ( pat, caseExpr ) ->
                                                    if i == caseIndex then
                                                        ( pat, replaceExprAtPath caseExpr restPath newExpr )

                                                    else
                                                        ( pat, caseExpr )
                                                )
                                }

                    LambdaExpression lambda ->
                        LambdaExpression { lambda | expression = replaceExprAtPath lambda.expression restPath newExpr }

                    Negation inner ->
                        Negation (replaceExprAtPath inner restPath newExpr)

                    RecordExpr setters ->
                        RecordExpr
                            (setters
                                |> List.indexedMap
                                    (\i (Node sRange ( name, valExpr )) ->
                                        if i == childIndex then
                                            Node sRange ( name, replaceExprAtPath valExpr restPath newExpr )

                                        else
                                            Node sRange ( name, valExpr )
                                    )
                            )

                    RecordUpdateExpression name setters ->
                        RecordUpdateExpression name
                            (setters
                                |> List.indexedMap
                                    (\i (Node sRange ( sName, valExpr )) ->
                                        if i == childIndex then
                                            Node sRange ( sName, replaceExprAtPath valExpr restPath newExpr )

                                        else
                                            Node sRange ( sName, valExpr )
                                    )
                            )

                    RecordAccess inner field ->
                        RecordAccess (replaceExprAtPath inner restPath newExpr) field

                    _ ->
                        expr
                )


replaceNth : Int -> (Node Expression -> List Int -> Node Expression -> Node Expression) -> List (Node Expression) -> List Int -> Node Expression -> List (Node Expression)
replaceNth targetIndex replaceFn nodes restPath newExpr =
    List.indexedMap
        (\i node ->
            if i == targetIndex then
                replaceFn node restPath newExpr

            else
                node
        )
        nodes


unwrapSingleApp : Expression -> Expression
unwrapSingleApp expr =
    case expr of
        Application [ single ] ->
            Node.value single

        other ->
            other



-- Operator tables


comparisonSwaps : String -> List String
comparisonSwaps op =
    case op of
        ">" ->
            [ ">=" ]

        ">=" ->
            [ ">" ]

        "<" ->
            [ "<=" ]

        "<=" ->
            [ "<" ]

        "==" ->
            [ "/=" ]

        "/=" ->
            [ "==" ]

        _ ->
            []


comparisonNegations : String -> List String
comparisonNegations op =
    case op of
        "<" ->
            [ ">=" ]

        "<=" ->
            [ ">" ]

        ">" ->
            [ "<=" ]

        ">=" ->
            [ "<" ]

        _ ->
            []


arithmeticSwaps : String -> List String
arithmeticSwaps op =
    case op of
        "+" ->
            [ "-" ]

        "-" ->
            [ "+" ]

        "*" ->
            [ "//" ]

        "//" ->
            [ "*" ]

        _ ->
            []


logicalSwaps : String -> List String
logicalSwaps op =
    case op of
        "&&" ->
            [ "||" ]

        "||" ->
            [ "&&" ]

        _ ->
            []


concatRemovalAst : String -> InfixDirection -> Node Expression -> Node Expression -> Range -> File -> Int -> List Int -> List Mutation
concatRemovalAst op dir left right range file declIndex path =
    if op == "++" then
        [ { line = range.start.row
          , column = range.start.column
          , operator = "concatRemoval"
          , description = "Kept only left side of `++`"
          , mutatedFile = replaceExpression file declIndex path left
          }
        , { line = range.start.row
          , column = range.start.column
          , operator = "concatRemoval"
          , description = "Kept only right side of `++`"
          , mutatedFile = replaceExpression file declIndex path right
          }
        ]

    else
        []


removeListElementMutations : List (Node Expression) -> Range -> File -> Int -> List Int -> List Mutation
removeListElementMutations elements range file declIndex path =
    let
        elementCount =
            List.length elements
    in
    List.indexedMap
        (\index _ ->
            let
                remaining =
                    elements
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( i, _ ) -> i /= index)
                        |> List.map Tuple.second
            in
            { line = range.start.row
            , column = range.start.column
            , operator = "removeListElement"
            , description = "Removed element " ++ String.fromInt (index + 1) ++ " of " ++ String.fromInt elementCount ++ " from list"
            , mutatedFile = replaceExpression file declIndex path (Node range (ListExpr remaining))
            }
        )
        elements


functionSwapTable : List ( ( List String, String ), ( List String, String ) )
functionSwapTable =
    [ ( ( [ "List" ], "head" ), ( [ "List" ], "last" ) )
    , ( ( [ "List" ], "sort" ), ( [ "List" ], "reverse" ) )
    , ( ( [ "List" ], "minimum" ), ( [ "List" ], "maximum" ) )
    , ( ( [ "List" ], "any" ), ( [ "List" ], "all" ) )
    , ( ( [ "List" ], "sum" ), ( [ "List" ], "product" ) )
    , ( ( [ "String" ], "toUpper" ), ( [ "String" ], "toLower" ) )
    , ( ( [ "String" ], "toFloat" ), ( [ "String" ], "toInt" ) )
    , ( ( [ "String" ], "left" ), ( [ "String" ], "right" ) )
    , ( ( [ "String" ], "trimLeft" ), ( [ "String" ], "trimRight" ) )
    , ( ( [ "String" ], "startsWith" ), ( [ "String" ], "endsWith" ) )
    , ( ( [ "String" ], "cons" ), ( [ "String" ], "append" ) )
    , ( ( [], "min" ), ( [], "max" ) )
    , ( ( [ "Basics" ], "min" ), ( [ "Basics" ], "max" ) )
    , ( ( [], "ceiling" ), ( [], "floor" ) )
    , ( ( [ "Basics" ], "ceiling" ), ( [ "Basics" ], "floor" ) )
    ]


functionSwapMutations : List String -> String -> Range -> List (Node Expression) -> Range -> File -> Int -> List Int -> List Mutation
functionSwapMutations fnMod fnName fnRange args appRange file declIndex path =
    functionSwapTable
        |> List.filterMap
            (\( ( fromMod, fromName ), ( toMod, toName ) ) ->
                if fromMod == fnMod && fromName == fnName then
                    Just ( toMod, toName )

                else if toMod == fnMod && toName == fnName then
                    Just ( fromMod, fromName )

                else
                    Nothing
            )
        |> List.filter (\( toMod, toName ) -> not (toMod == fnMod && toName == fnName))
        |> List.map
            (\( toMod, toName ) ->
                let
                    fromQualified =
                        if List.isEmpty fnMod then
                            fnName

                        else
                            String.join "." fnMod ++ "." ++ fnName

                    toQualified =
                        if List.isEmpty toMod then
                            toName

                        else
                            String.join "." toMod ++ "." ++ toName
                in
                { line = fnRange.start.row
                , column = fnRange.start.column
                , operator = "functionSwap"
                , description = "Swapped `" ++ fromQualified ++ "` to `" ++ toQualified ++ "`"
                , mutatedFile =
                    replaceExpression file declIndex path
                        (Node appRange
                            (Application
                                (Node fnRange (FunctionOrValue toMod toName) :: args)
                            )
                        )
                }
            )
