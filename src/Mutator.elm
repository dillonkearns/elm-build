module Mutator exposing (Mutation, applyMutation, extractOriginalText, generateMutations, generateMutationsFromFile, hashKey, writeFile)

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
import Elm.Syntax.Pattern
import Elm.Syntax.Range as Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Elm.Writer


type alias Mutation =
    { line : Int
    , column : Int
    , operator : String
    , description : String
    , mutatedFile : File
    , spliceRange : Range
    , spliceText : String
    }


{-| Parse source and generate all mutations.
-}
generateMutations : String -> List Mutation
generateMutations source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            generateMutationsFromFile source file
                |> filterNoOps source

        Err _ ->
            []


{-| Remove mutations where the replacement text is identical to the original.
Central guard against no-op mutations from any operator.
-}
filterNoOps : String -> List Mutation -> List Mutation
filterNoOps source mutations =
    List.filter
        (\m -> extractSourceRange source m.spliceRange /= m.spliceText)
        mutations


{-| Generate all mutations from an already-parsed File AST.
-}
generateMutationsFromFile : String -> File -> List Mutation
generateMutationsFromFile source file =
    file.declarations
        |> List.indexedMap
            (\declIndex (Node declRange decl) ->
                findMutationsInDeclaration source file declIndex decl
            )
        |> List.concat


{-| Write a File AST back to source string (for display/debugging).
-}
writeFile : File -> String
writeFile file =
    Elm.Writer.write (Elm.Writer.writeFile file)


{-| Compute a deterministic hash key for a mutation.

Uses the splice range and replacement text to uniquely identify the mutation
without serializing the full File AST via Elm.Writer.

-}
hashKey : Mutation -> String
hashKey mutation =
    mutation.operator
        ++ "@"
        ++ String.fromInt mutation.spliceRange.start.row
        ++ ":"
        ++ String.fromInt mutation.spliceRange.start.column
        ++ "-"
        ++ String.fromInt mutation.spliceRange.end.row
        ++ ":"
        ++ String.fromInt mutation.spliceRange.end.column
        ++ "|"
        ++ mutation.spliceText


{-| Extract the original text at a mutation's splice range from the source.
Useful for detecting no-op mutations (where replacement equals original).
-}
extractOriginalText : String -> Mutation -> String
extractOriginalText source mutation =
    extractSourceRange source mutation.spliceRange


{-| Apply a mutation to the original source by splicing the replacement
text into the source at the mutation's range. This avoids rewriting
the entire file via Elm.Writer (which breaks multi-line exposing lists).
-}
applyMutation : String -> Mutation -> String
applyMutation originalSource mutation =
    spliceSource originalSource mutation.spliceRange mutation.spliceText


{-| Replace text in source at the given Range with replacement text.

Ranges use 1-based row and column numbers (elm-syntax convention).

-}
spliceSource : String -> Range -> String -> String
spliceSource source range replacement =
    let
        lines =
            String.lines source

        -- Get everything before the range start
        beforeLines =
            List.take (range.start.row - 1) lines

        startLine =
            lines |> List.drop (range.start.row - 1) |> List.head |> Maybe.withDefault ""

        beforeText =
            String.left (range.start.column - 1) startLine

        -- Get everything after the range end
        endLine =
            lines |> List.drop (range.end.row - 1) |> List.head |> Maybe.withDefault ""

        afterText =
            String.dropLeft (range.end.column - 1) endLine

        afterLines =
            List.drop range.end.row lines
    in
    (beforeLines
        ++ [ beforeText ++ replacement ++ afterText ]
        ++ afterLines
    )
        |> String.join "\n"


findMutationsInDeclaration : String -> File -> Int -> Declaration -> List Mutation
findMutationsInDeclaration source file declIndex decl =
    case decl of
        FunctionDeclaration function ->
            let
                (Node _ impl) =
                    function.declaration

                (Node exprRange _) =
                    impl.expression

                expressionMutations =
                    findMutationsInExpression source file declIndex [] impl.expression

                extremeMutations =
                    case function.signature of
                        Just (Node _ sig) ->
                            case defaultForType (Node.value sig.typeAnnotation) of
                                Just ( defaultExpr, defaultText ) ->
                                    [ { line = exprRange.start.row
                                      , column = exprRange.start.column
                                      , operator = "extremeMutation"
                                      , description = "Replaced function body with default value"
                                      , mutatedFile = replaceExpression file declIndex [] (Node exprRange defaultExpr)
                                      , spliceRange = exprRange
                                      , spliceText = defaultText
                                      }
                                    ]

                                Nothing ->
                                    []

                        Nothing ->
                            []
            in
            extremeMutations ++ expressionMutations

        _ ->
            []


{-| Get a default value for a type annotation's return type.
Walks through function arrows to find the final return type.
Returns the default Expression AST and its text representation.
-}
defaultForType : TypeAnnotation -> Maybe ( Expression, String )
defaultForType typeAnnotation =
    case typeAnnotation of
        -- Function type: recurse into the return type
        FunctionTypeAnnotation _ (Node _ returnType) ->
            defaultForType returnType

        -- Known types with sensible defaults
        Typed (Node _ ( [], "Int" )) _ ->
            Just ( Integer 0, "0" )

        Typed (Node _ ( [], "Float" )) _ ->
            Just ( Floatable 0, "0.0" )

        Typed (Node _ ( [], "String" )) _ ->
            Just ( Literal "", "\"\"" )

        Typed (Node _ ( [], "Bool" )) _ ->
            Just ( FunctionOrValue [] "False", "False" )

        Typed (Node _ ( [], "List" )) _ ->
            Just ( ListExpr [], "[]" )

        Typed (Node _ ( [], "Maybe" )) _ ->
            Just ( FunctionOrValue [] "Nothing", "Nothing" )

        -- Qualified versions
        Typed (Node _ ( [ "Basics" ], "Int" )) _ ->
            Just ( Integer 0, "0" )

        Typed (Node _ ( [ "Basics" ], "Float" )) _ ->
            Just ( Floatable 0, "0.0" )

        Typed (Node _ ( [ "Basics" ], "Bool" )) _ ->
            Just ( FunctionOrValue [] "False", "False" )

        Typed (Node _ ( [ "String" ], "String" )) _ ->
            Just ( Literal "", "\"\"" )

        -- Result -> Err "mutation"
        Typed (Node _ ( [], "Result" )) _ ->
            Just ( Application [ Node Range.empty (FunctionOrValue [] "Err"), Node Range.empty (Literal "extreme mutation") ], "Err \"extreme mutation\"" )

        _ ->
            Nothing


{-| Walk the expression AST and collect mutations.

Each mutation stores the full modified File AST — the original file with
one expression node replaced.

-}
findMutationsInExpression : String -> File -> Int -> List Int -> Node Expression -> List Mutation
findMutationsInExpression source file declIndex path (Node range expr) =
    let
        recurse : List (Node Expression) -> List Mutation
        recurse nodes =
            nodes
                |> List.indexedMap
                    (\i node ->
                        findMutationsInExpression source file declIndex (path ++ [ i ]) node
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

        -- Create a mutation by replacing the current expression with a new one.
        -- Uses Elm.Writer for the replacement text (works for single-line expressions).
        mutate : String -> String -> Expression -> Mutation
        mutate operator description newExpr =
            let
                replacement =
                    Node range newExpr
            in
            { line = range.start.row
            , column = range.start.column
            , operator = operator
            , description = description
            , mutatedFile = replaceExpression file declIndex path replacement
            , spliceRange = range
            , spliceText = Elm.Writer.write (Elm.Writer.writeExpression replacement)
            }

        -- Create a mutation with explicit splice range and text.
        -- Used for multi-line expressions where Elm.Writer would break formatting.
        mutateWithSplice : String -> String -> Expression -> Range -> String -> Mutation
        mutateWithSplice operator description newExpr sRange sText =
            { line = range.start.row
            , column = range.start.column
            , operator = operator
            , description = description
            , mutatedFile = replaceExpression file declIndex path (Node range newExpr)
            , spliceRange = sRange
            , spliceText = sText
            }

        thisMutation : List Mutation
        thisMutation =
            case expr of
                IfBlock (Node condRange condExpr) (Node thenRange thenExpr) (Node elseRange elseExpr) ->
                    let
                        condText =
                            extractSourceRange source condRange

                        negatedCondText =
                            case condExpr of
                                FunctionOrValue _ _ ->
                                    "not " ++ condText

                                _ ->
                                    "not (" ++ condText ++ ")"
                    in
                    [ -- negateCondition: splice just the condition
                      mutateWithSplice "negateCondition"
                        "Negated if-condition"
                        (IfBlock
                            (Node condRange
                                (Application
                                    [ Node Range.empty (FunctionOrValue [] "not")
                                    , (case condExpr of
                                        FunctionOrValue _ _ -> Node condRange condExpr
                                        _ -> Node condRange (ParenthesizedExpression (Node condRange condExpr))
                                      )
                                    ]
                                )
                            )
                            (Node thenRange thenExpr)
                            (Node elseRange elseExpr)
                        )
                        condRange
                        negatedCondText

                    -- dropElseBranch: splice just the else branch with then branch text
                    , mutateWithSplice "dropElseBranch"
                        "Replaced else branch with then branch"
                        (IfBlock (Node condRange condExpr) (Node thenRange thenExpr) (Node elseRange thenExpr))
                        elseRange
                        (extractSourceRange source thenRange)

                    -- conditionalTrue: splice just the condition
                    , mutateWithSplice "conditionalTrue"
                        "Replaced condition with `True`"
                        (IfBlock (Node condRange (FunctionOrValue [] "True")) (Node thenRange thenExpr) (Node elseRange elseExpr))
                        condRange
                        "True"

                    -- conditionalFalse: splice just the condition
                    , mutateWithSplice "conditionalFalse"
                        "Replaced condition with `False`"
                        (IfBlock (Node condRange (FunctionOrValue [] "False")) (Node thenRange thenExpr) (Node elseRange elseExpr))
                        condRange
                        "False"
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
                        ++ concatRemovalAst source op dir left right range file declIndex path
                        ++ pipelineRemoval source op left right range file declIndex path

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
                    functionSwapMutations source fnMod fnName fnRange args range file declIndex path
                        ++ (case ( fnMod, fnName, args ) of
                                ( [], "not", [ Node argRange _ ] ) ->
                                    [ mutateWithSplice "removeNot" "Removed `not` call"
                                        (Application args |> unwrapSingleApp)
                                        range
                                        (extractSourceRange source argRange)
                                    ]

                                ( [], "Just", _ ) ->
                                    [ mutate "replaceWithNothing" "Replaced `Just ...` with `Nothing`" (FunctionOrValue [] "Nothing") ]

                                ( [ "Maybe" ], "Just", _ ) ->
                                    [ mutate "replaceWithNothing" "Replaced `Just ...` with `Nothing`" (FunctionOrValue [] "Nothing") ]

                                _ ->
                                    []
                           )

                Negation (Node innerRange innerExpr) ->
                    [ mutateWithSplice "removeNegation" "Removed unary negation"
                        innerExpr
                        range
                        (extractSourceRange source innerRange)
                    ]

                ListExpr elements ->
                    if List.length elements >= 2 then
                        removeListElementMutations source elements range file declIndex path
                            ++ [ mutate "emptyList" "Replaced list with `[]`" (ListExpr []) ]

                    else
                        []

                CaseExpression caseBlock ->
                    if List.length caseBlock.cases >= 2 then
                        swapCaseBranchMutations source caseBlock range file declIndex path

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


concatRemovalAst : String -> String -> InfixDirection -> Node Expression -> Node Expression -> Range -> File -> Int -> List Int -> List Mutation
concatRemovalAst source op dir (Node leftRange _) (Node rightRange _) range file declIndex path =
    if op == "++" then
        [ { line = range.start.row
          , column = range.start.column
          , operator = "concatRemoval"
          , description = "Kept only left side of `++`"
          , mutatedFile = replaceExpression file declIndex path (Node leftRange (FunctionOrValue [] "placeholder__"))
          , spliceRange = range
          , spliceText = extractSourceRange source leftRange
          }
        , { line = range.start.row
          , column = range.start.column
          , operator = "concatRemoval"
          , description = "Kept only right side of `++`"
          , mutatedFile = replaceExpression file declIndex path (Node rightRange (FunctionOrValue [] "placeholder__"))
          , spliceRange = range
          , spliceText = extractSourceRange source rightRange
          }
        ]

    else
        []


{-| For `a |> f`, generate a mutation that removes the pipeline step,
keeping just `a`. Tests whether the pipeline step matters.
-}
pipelineRemoval : String -> String -> Node Expression -> Node Expression -> Range -> File -> Int -> List Int -> List Mutation
pipelineRemoval source op (Node leftRange _) (Node rightRange _) range file declIndex path =
    if op == "|>" then
        [ { line = range.start.row
          , column = range.start.column
          , operator = "removePipelineStep"
          , description = "Removed `|>` pipeline step"
          , mutatedFile = replaceExpression file declIndex path (Node leftRange (FunctionOrValue [] "placeholder__"))
          , spliceRange = range
          , spliceText = extractSourceRange source leftRange
          }
        ]

    else if op == "<|" then
        [ { line = range.start.row
          , column = range.start.column
          , operator = "removePipelineStep"
          , description = "Removed `<|` pipeline step"
          , mutatedFile = replaceExpression file declIndex path (Node rightRange (FunctionOrValue [] "placeholder__"))
          , spliceRange = range
          , spliceText = extractSourceRange source rightRange
          }
        ]

    else
        []


removeListElementMutations : String -> List (Node Expression) -> Range -> File -> Int -> List Int -> List Mutation
removeListElementMutations source elements range file declIndex path =
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

                newExpr =
                    Node range (ListExpr remaining)

                -- Build splice text by extracting remaining elements from original source
                remainingTexts =
                    remaining
                        |> List.map (\(Node r _) -> extractSourceRange source r)

                listText =
                    "[ " ++ String.join ", " remainingTexts ++ " ]"
            in
            { line = range.start.row
            , column = range.start.column
            , operator = "removeListElement"
            , description = "Removed element " ++ String.fromInt (index + 1) ++ " of " ++ String.fromInt elementCount ++ " from list"
            , mutatedFile = replaceExpression file declIndex path newExpr
            , spliceRange = range
            , spliceText = listText
            }
        )
        elements


{-| Generate mutations that swap bodies of adjacent case branches.
For N branches, generates N-1 mutations (each swaps branch i with branch i+1).
Uses source extraction to preserve original formatting.
-}
swapCaseBranchMutations :
    String
    -> { expression : Node Expression, cases : List ( Node Elm.Syntax.Pattern.Pattern, Node Expression ) }
    -> Range
    -> File
    -> Int
    -> List Int
    -> List Mutation
swapCaseBranchMutations source caseBlock range file declIndex path =
    let
        cases =
            caseBlock.cases

        pairs =
            List.map2 Tuple.pair cases (List.drop 1 cases)
    in
    List.indexedMap
        (\i ( ( pat1, Node expr1Range _ ), ( pat2, Node expr2Range _ ) ) ->
            let
                expr2Text =
                    extractSourceRange source expr2Range

                swappedCases =
                    cases
                        |> List.indexedMap
                            (\j ( pat, expr ) ->
                                if j == i then
                                    ( pat, Tuple.second (Maybe.withDefault ( pat, expr ) (List.head (List.drop (i + 1) cases))) )

                                else if j == i + 1 then
                                    ( pat, Tuple.second (Maybe.withDefault ( pat, expr ) (List.head (List.drop i cases))) )

                                else
                                    ( pat, expr )
                            )
            in
            { line = expr1Range.start.row
            , column = expr1Range.start.column
            , operator = "swapCaseBranches"
            , description =
                "Swapped case branch "
                    ++ String.fromInt (i + 1)
                    ++ " with branch "
                    ++ String.fromInt (i + 2)
            , mutatedFile =
                replaceExpression file declIndex path
                    (Node range (CaseExpression { caseBlock | cases = swappedCases }))
            , spliceRange = expr1Range
            , spliceText = expr2Text
            }
        )
        pairs


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


functionSwapMutations : String -> List String -> String -> Range -> List (Node Expression) -> Range -> File -> Int -> List Int -> List Mutation
functionSwapMutations source fnMod fnName fnRange args appRange file declIndex path =
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
                let
                    newExpr =
                        Node appRange
                            (Application
                                (Node fnRange (FunctionOrValue toMod toName) :: args)
                            )
                in
                { line = fnRange.start.row
                , column = fnRange.start.column
                , operator = "functionSwap"
                , description = "Swapped `" ++ fromQualified ++ "` to `" ++ toQualified ++ "`"
                , mutatedFile = replaceExpression file declIndex path newExpr
                , spliceRange = fnRange
                , spliceText = toQualified
                }
            )


{-| Extract the text from a source string at the given Range.
-}
extractSourceRange : String -> Range -> String
extractSourceRange source range =
    let
        lines =
            String.lines source
    in
    if range.start.row == range.end.row then
        -- Single line: extract from start column to end column
        lines
            |> List.drop (range.start.row - 1)
            |> List.head
            |> Maybe.withDefault ""
            |> String.slice (range.start.column - 1) (range.end.column - 1)

    else
        -- Multi-line: first line from start column, middle lines fully, last line to end column
        let
            firstLine =
                lines
                    |> List.drop (range.start.row - 1)
                    |> List.head
                    |> Maybe.withDefault ""
                    |> String.dropLeft (range.start.column - 1)

            middleLines =
                lines
                    |> List.drop range.start.row
                    |> List.take (range.end.row - range.start.row - 1)

            lastLine =
                lines
                    |> List.drop (range.end.row - 1)
                    |> List.head
                    |> Maybe.withDefault ""
                    |> String.left (range.end.column - 1)
        in
        (firstLine :: middleLines ++ [ lastLine ])
            |> String.join "\n"
