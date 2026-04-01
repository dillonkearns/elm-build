module TestAnalysis exposing (discoverTestValues, discoverTestValuesViaInterpreter, getCandidateNames, probeCandidate, usesFuzz)

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
import Eval.Module
import Set exposing (Set)
import Types


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


{-| Get all exposed zero-argument value names from a module.

These are candidates for Test discovery — we'll try evaluating each one
via the interpreter to see if it's a Test.

-}
getCandidateNames : String -> List String
getCandidateNames source =
    case Elm.Parser.parseToFile source of
        Err _ ->
            []

        Ok file ->
            let
                exposedNames =
                    getExposedNames file
            in
            file.declarations
                |> List.filterMap (isZeroArgDeclaration >> Maybe.andThen (filterExposed exposedNames))


{-| Check if a declaration is a zero-argument function (a value, not a function).
Returns the name if so.
-}
isZeroArgDeclaration : Node Declaration -> Maybe String
isZeroArgDeclaration (Node _ decl) =
    case decl of
        FunctionDeclaration function ->
            let
                (Node _ impl) =
                    function.declaration
            in
            if List.isEmpty impl.arguments then
                Just (Node.value impl.name)

            else
                Nothing

        _ ->
            Nothing


{-| Discover Test values by evaluating candidates via the interpreter.

For each candidate name, tries `SimpleTestRunner.runToString ModuleName.name`.
If the interpreter returns a String result, the value is a Test.
This is 100% accurate — no heuristics, no type annotation needed.

-}
discoverTestValuesViaInterpreter : Eval.Module.ProjectEnv -> String -> List String -> List String
discoverTestValuesViaInterpreter projectEnv testModuleName candidateNames =
    candidateNames
        |> List.filter
            (\name ->
                case probeCandidate projectEnv testModuleName name of
                    Ok _ ->
                        True

                    Err _ ->
                        False
            )


{-| Try evaluating a candidate value as a Test.

Returns Ok if it's a Test (with the result string), or Err with a reason why not.

-}
probeCandidate : Eval.Module.ProjectEnv -> String -> String -> Result String String
probeCandidate projectEnv testModuleName name =
    let
        probeWrapper =
            String.join "\n"
                [ "module Probe__ exposing (probe__)"
                , "import SimpleTestRunner"
                , "import " ++ testModuleName
                , ""
                , "probe__ : String"
                , "probe__ ="
                , "    SimpleTestRunner.runToString " ++ testModuleName ++ "." ++ name
                , ""
                ]

        result =
            Eval.Module.evalWithEnv
                projectEnv
                [ probeWrapper ]
                (Elm.Syntax.Expression.FunctionOrValue [] "probe__")
    in
    case result of
        Ok (Types.String s) ->
            Ok s

        Ok _ ->
            Err "returned non-String value"

        Err (Types.ParsingError _) ->
            Err "could not parse probe wrapper"

        Err (Types.EvalError evalErr) ->
            Err ("eval error: " ++ evalErrorToString evalErr)


evalErrorToString : Types.EvalErrorData -> String
evalErrorToString err =
    case err.error of
        Types.NameError name ->
            "could not resolve '" ++ name ++ "'"

        Types.TypeError msg ->
            "type error: " ++ msg

        Types.Unsupported msg ->
            "unsupported: " ++ msg

        Types.Todo msg ->
            "hit Debug.todo: " ++ msg


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
