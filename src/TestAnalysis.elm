module TestAnalysis exposing (usesFuzz)

{-| Static analysis to detect whether an Elm test module uses fuzz tests.

Parses the source with elm-syntax and walks expression ASTs to find references
to `Test.fuzz`, `Test.fuzz2`, or `Test.fuzz3`, accounting for import aliases
and exposing lists.

-}

import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Set exposing (Set)


{-| Detect whether an Elm source file uses fuzz tests by checking for references
to Test.fuzz, Test.fuzz2, or Test.fuzz3.

Returns False if the source fails to parse.

-}
usesFuzz : String -> Bool
usesFuzz source =
    case Elm.Parser.parseToFile source of
        Err _ ->
            False

        Ok file ->
            let
                fuzzRefs =
                    buildFuzzRefs file.imports
            in
            List.any (declarationUsesFuzz fuzzRefs) file.declarations


fuzzNames : Set String
fuzzNames =
    Set.fromList [ "fuzz", "fuzz2", "fuzz3" ]


buildFuzzRefs : List (Node Import) -> Set ( List String, String )
buildFuzzRefs imports =
    List.foldl addFuzzRefsFromImport Set.empty imports


addFuzzRefsFromImport : Node Import -> Set ( List String, String ) -> Set ( List String, String )
addFuzzRefsFromImport (Node _ import_) refs =
    let
        moduleName =
            Node.value import_.moduleName
    in
    if moduleName /= [ "Test" ] then
        refs

    else
        let
            qualifier =
                case import_.moduleAlias of
                    Just (Node _ alias_) ->
                        alias_

                    Nothing ->
                        moduleName

            withQualified =
                Set.foldl
                    (\name acc -> Set.insert ( qualifier, name ) acc)
                    refs
                    fuzzNames

            unqualifiedNames =
                case import_.exposingList of
                    Nothing ->
                        Set.empty

                    Just (Node _ (All _)) ->
                        fuzzNames

                    Just (Node _ (Explicit exposes)) ->
                        exposes
                            |> List.filterMap
                                (\(Node _ expose) ->
                                    case expose of
                                        FunctionExpose name ->
                                            if Set.member name fuzzNames then
                                                Just name

                                            else
                                                Nothing

                                        _ ->
                                            Nothing
                                )
                            |> Set.fromList
        in
        Set.foldl
            (\name acc -> Set.insert ( [], name ) acc)
            withQualified
            unqualifiedNames


declarationUsesFuzz : Set ( List String, String ) -> Node Declaration -> Bool
declarationUsesFuzz fuzzRefs (Node _ decl) =
    case decl of
        FunctionDeclaration func ->
            let
                (Node _ impl) =
                    func.declaration
            in
            expressionUsesFuzz fuzzRefs impl.expression

        Destructuring _ expr ->
            expressionUsesFuzz fuzzRefs expr

        _ ->
            False


expressionUsesFuzz : Set ( List String, String ) -> Node Expression -> Bool
expressionUsesFuzz fuzzRefs (Node _ expr) =
    case expr of
        FunctionOrValue moduleName name ->
            Set.member ( moduleName, name ) fuzzRefs

        Application exprs ->
            List.any (expressionUsesFuzz fuzzRefs) exprs

        OperatorApplication _ _ left right ->
            expressionUsesFuzz fuzzRefs left || expressionUsesFuzz fuzzRefs right

        IfBlock cond true false ->
            expressionUsesFuzz fuzzRefs cond
                || expressionUsesFuzz fuzzRefs true
                || expressionUsesFuzz fuzzRefs false

        Negation child ->
            expressionUsesFuzz fuzzRefs child

        TupledExpression exprs ->
            List.any (expressionUsesFuzz fuzzRefs) exprs

        ParenthesizedExpression child ->
            expressionUsesFuzz fuzzRefs child

        LetExpression letBlock ->
            expressionUsesFuzz fuzzRefs letBlock.expression
                || List.any (letDeclarationUsesFuzz fuzzRefs) letBlock.declarations

        CaseExpression caseBlock ->
            expressionUsesFuzz fuzzRefs caseBlock.expression
                || List.any (\( _, caseExpr ) -> expressionUsesFuzz fuzzRefs caseExpr) caseBlock.cases

        LambdaExpression lambda ->
            expressionUsesFuzz fuzzRefs lambda.expression

        RecordExpr fields ->
            List.any (\(Node _ ( _, valueExpr )) -> expressionUsesFuzz fuzzRefs valueExpr) fields

        ListExpr elements ->
            List.any (expressionUsesFuzz fuzzRefs) elements

        RecordAccess child _ ->
            expressionUsesFuzz fuzzRefs child

        RecordUpdateExpression _ setters ->
            List.any (\(Node _ ( _, valueExpr )) -> expressionUsesFuzz fuzzRefs valueExpr) setters

        _ ->
            False


letDeclarationUsesFuzz : Set ( List String, String ) -> Node LetDeclaration -> Bool
letDeclarationUsesFuzz fuzzRefs (Node _ letDecl) =
    case letDecl of
        LetFunction func ->
            let
                (Node _ impl) =
                    func.declaration
            in
            expressionUsesFuzz fuzzRefs impl.expression

        LetDestructuring _ expr ->
            expressionUsesFuzz fuzzRefs expr
