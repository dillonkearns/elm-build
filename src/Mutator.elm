module Mutator exposing (Mutation, generateMutations)

{-| Mutation generator for Elm source code.

Uses elm-syntax to parse source into an AST, find mutable expressions,
and generate mutations by applying string-level replacements guided by
AST range information. This preserves original formatting everywhere
except the mutation point.

-}

import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)


type alias Mutation =
    { line : Int
    , column : Int
    , operator : String
    , description : String
    , mutatedSource : String
    }


generateMutations : String -> List Mutation
generateMutations source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            file.declarations
                |> List.concatMap (\(Node _ decl) -> findMutationsInDeclaration source decl)

        Err _ ->
            []


findMutationsInDeclaration : String -> Declaration -> List Mutation
findMutationsInDeclaration source decl =
    case decl of
        FunctionDeclaration function ->
            let
                (Node _ impl) =
                    function.declaration
            in
            findMutationsInExpression source impl.expression

        _ ->
            []


findMutationsInExpression : String -> Node Expression -> List Mutation
findMutationsInExpression source (Node range expr) =
    let
        recurse : List (Node Expression) -> List Mutation
        recurse nodes =
            List.concatMap (findMutationsInExpression source) nodes

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

        thisMutation : List Mutation
        thisMutation =
            case expr of
                IfBlock (Node condRange condExpr) _ _ ->
                    [ negateCondition source condRange condExpr ]

                OperatorApplication op _ left right ->
                    (comparisonSwaps op
                        |> List.map (operatorMutation "swapComparison" source left right op)
                    )
                        ++ (arithmeticSwaps op
                                |> List.map (operatorMutation "replaceArithmetic" source left right op)
                           )
                        ++ (logicalSwaps op
                                |> List.map (operatorMutation "swapLogicalOperator" source left right op)
                           )

                FunctionOrValue [] "True" ->
                    [ swapBooleanLiteral source range "True" "False" ]

                FunctionOrValue [] "False" ->
                    [ swapBooleanLiteral source range "False" "True" ]

                Integer n ->
                    [ replaceIntLiteral source range n ]

                Literal s ->
                    [ replaceStringLiteral source range s ]

                _ ->
                    []
    in
    thisMutation ++ childMutations



-- Mutation operators


negateCondition : String -> Range -> Expression -> Mutation
negateCondition source condRange condExpr =
    let
        condText =
            extractRange source condRange

        replacement =
            case condExpr of
                FunctionOrValue _ _ ->
                    "not " ++ condText

                _ ->
                    "not (" ++ condText ++ ")"
    in
    { line = condRange.start.row
    , column = condRange.start.column
    , operator = "negateCondition"
    , description = "Changed `if " ++ condText ++ "` to `if " ++ replacement ++ "`"
    , mutatedSource = replaceRange source condRange replacement
    }


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

        "++" ->
            []

        "::" ->
            []

        _ ->
            []


swapBooleanLiteral : String -> Range -> String -> String -> Mutation
swapBooleanLiteral source range oldVal newVal =
    { line = range.start.row
    , column = range.start.column
    , operator = "swapBooleanLiteral"
    , description = "Changed `" ++ oldVal ++ "` to `" ++ newVal ++ "`"
    , mutatedSource = replaceRange source range newVal
    }


logicalSwaps : String -> List String
logicalSwaps op =
    case op of
        "&&" ->
            [ "||" ]

        "||" ->
            [ "&&" ]

        _ ->
            []


replaceIntLiteral : String -> Range -> Int -> Mutation
replaceIntLiteral source range n =
    let
        replacement =
            if n == 0 then
                "1"

            else if n == 1 then
                "0"

            else
                "0"
    in
    { line = range.start.row
    , column = range.start.column
    , operator = "replaceIntLiteral"
    , description = "Changed `" ++ String.fromInt n ++ "` to `" ++ replacement ++ "`"
    , mutatedSource = replaceRange source range replacement
    }


replaceStringLiteral : String -> Range -> String -> Mutation
replaceStringLiteral source range s =
    let
        replacement =
            if String.isEmpty s then
                "\"mutated!\""

            else
                "\"\""
    in
    { line = range.start.row
    , column = range.start.column
    , operator = "replaceStringLiteral"
    , description =
        if String.isEmpty s then
            "Changed `\"\"` to `\"mutated!\"`"

        else
            "Changed `\"" ++ s ++ "\"` to `\"\"`"
    , mutatedSource = replaceRange source range replacement
    }


operatorMutation : String -> String -> Node Expression -> Node Expression -> String -> String -> Mutation
operatorMutation operatorName source (Node leftRange _) (Node rightRange _) oldOp newOp =
    let
        ( _, leftEnd ) =
            rangeToOffsets source leftRange

        ( rightStart, _ ) =
            rangeToOffsets source rightRange

        gap =
            String.slice leftEnd rightStart source

        newGap =
            String.replace oldOp newOp gap
    in
    { line = leftRange.start.row
    , column = leftRange.start.column
    , operator = operatorName
    , description = "Changed `" ++ oldOp ++ "` to `" ++ newOp ++ "`"
    , mutatedSource = String.left leftEnd source ++ newGap ++ String.dropLeft rightStart source
    }



-- String manipulation helpers


rangeToOffsets : String -> Range -> ( Int, Int )
rangeToOffsets source { start, end } =
    let
        lines =
            String.lines source

        lineOffset row =
            lines
                |> List.take (row - 1)
                |> List.map (\l -> String.length l + 1)
                |> List.sum
    in
    ( lineOffset start.row + (start.column - 1)
    , lineOffset end.row + (end.column - 1)
    )


extractRange : String -> Range -> String
extractRange source range =
    let
        ( startOff, endOff ) =
            rangeToOffsets source range
    in
    String.slice startOff endOff source


replaceRange : String -> Range -> String -> String
replaceRange source range replacement =
    let
        ( startOff, endOff ) =
            rangeToOffsets source range
    in
    String.left startOff source ++ replacement ++ String.dropLeft endOff source
