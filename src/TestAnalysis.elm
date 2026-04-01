module TestAnalysis exposing (discoverTestValues, usesFuzz)

{-| Static analysis for Elm test modules.

Provides:

  - `usesFuzz` — detect whether a module uses fuzz tests
  - `discoverTestValues` — find exposed values of type `Test`

-}

import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Set exposing (Set)


{-| Discover exposed values of type `Test` in a module.

Parses the source, finds all top-level functions with a `Test` or `Test.Test`
type annotation, and returns only those that are in the module's exposing list.

-}
discoverTestValues : String -> List String
discoverTestValues source =
    case Elm.Parser.parseToFile source of
        Err _ ->
            []

        Ok file ->
            let
                exposedNames =
                    getExposedNames file

                testValueNames =
                    file.declarations
                        |> List.filterMap (isTestDeclaration >> Maybe.andThen (filterExposed exposedNames))
            in
            testValueNames


{-| Get the set of explicitly exposed names, or Nothing if exposing (..).
-}
getExposedNames : File -> Maybe (Set String)
getExposedNames file =
    case Node.value file.moduleDefinition of
        Elm.Syntax.Module.NormalModule { exposingList } ->
            exposingListToNames exposingList

        Elm.Syntax.Module.PortModule { exposingList } ->
            exposingListToNames exposingList

        Elm.Syntax.Module.EffectModule { exposingList } ->
            exposingListToNames exposingList


exposingListToNames : Node Exposing -> Maybe (Set String)
exposingListToNames (Node _ exposing_) =
    case exposing_ of
        All _ ->
            Nothing

        Explicit exposes ->
            exposes
                |> List.filterMap
                    (\(Node _ expose) ->
                        case expose of
                            FunctionExpose name ->
                                Just name

                            _ ->
                                Nothing
                    )
                |> Set.fromList
                |> Just


{-| Check if a declaration is a function with Test type annotation.
Returns the function name if so.
-}
isTestDeclaration : Node Declaration -> Maybe String
isTestDeclaration (Node _ decl) =
    case decl of
        FunctionDeclaration function ->
            case function.signature of
                Just (Node _ sig) ->
                    if isTestType (Node.value sig.typeAnnotation) then
                        Just (Node.value sig.name)

                    else
                        Nothing

                Nothing ->
                    Nothing

        _ ->
            Nothing


isTestType : TypeAnnotation -> Bool
isTestType typeAnnotation =
    case typeAnnotation of
        Typed (Node _ ( [], "Test" )) _ ->
            True

        Typed (Node _ ( [ "Test" ], "Test" )) _ ->
            True

        _ ->
            False


filterExposed : Maybe (Set String) -> String -> Maybe String
filterExposed maybeExposedNames name =
    case maybeExposedNames of
        Nothing ->
            -- exposing (..) — everything is exposed
            Just name

        Just exposedNames ->
            if Set.member name exposedNames then
                Just name

            else
                Nothing


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
