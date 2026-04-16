module TestAnalysis exposing (countRunnersStatically, discoverTestValues, discoverTestValuesViaInterpreter, extractDescribeChildren, extractDescribeChildrenAsExpressions, getCandidateNames, probeCandidate, usesFuzz)

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
import Elm.Syntax.Module exposing (Module(..))
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
discoverTestValuesViaInterpreter : Eval.Module.ProjectEnv -> String -> List String -> List String -> List String
discoverTestValuesViaInterpreter projectEnv testModuleName candidateNames extraSources =
    candidateNames
        |> List.filter
            (\name ->
                case probeCandidate projectEnv testModuleName name extraSources of
                    Ok _ ->
                        True

                    Err _ ->
                        False
            )


{-| Try evaluating a candidate value as a Test.

Returns Ok if it's a Test (with the result string), or Err with a reason why not.

-}
probeCandidate : Eval.Module.ProjectEnv -> String -> String -> List String -> Result String String
probeCandidate projectEnv testModuleName name extraSources =
    let
        -- Use `countTests`, not `runToString`: we only need to confirm the
        -- value is a Test (fromTest will type-error at runtime otherwise),
        -- not actually run any test bodies. Running full suites through the
        -- interpreter blows the JS call stack on large test modules.
        probeWrapper =
            String.join "\n"
                [ "module Probe__ exposing (probe__)"
                , "import SimpleTestRunner"
                , "import " ++ testModuleName
                , ""
                , "probe__ : String"
                , "probe__ ="
                , "    SimpleTestRunner.countTests " ++ testModuleName ++ "." ++ name
                , ""
                ]

        result =
            Eval.Module.evalWithEnvAndLimit
                (Just 5000000)
                projectEnv
                (extraSources ++ [ probeWrapper ])
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

        Types.TailCall _ ->
            "internal TCO signal"


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


{-| Extract individual test children from a `suite = describe "..." [child1, child2, ...]`
declaration. Returns each child as a source string substring extracted from the original
source using the AST node's Range.

This is the key to Salsa-style per-test caching: each child becomes an independent
cached computation whose semantic hash only includes ITS specific dependencies.
A mutation on function F only invalidates children that reference F.

Returns Nothing if the suite isn't a simple `describe` with a list literal.

-}
extractDescribeChildren : String -> String -> Maybe (List String)
extractDescribeChildren valueName source =
    case Elm.Parser.parseToFile source of
        Err _ ->
            Nothing

        Ok file ->
            let
                modName =
                    case Node.value file.moduleDefinition of
                        NormalModule { moduleName } ->
                            Node.value moduleName |> String.join "."

                        PortModule { moduleName } ->
                            Node.value moduleName |> String.join "."

                        EffectModule { moduleName } ->
                            Node.value moduleName |> String.join "."
            in
            file.declarations
                |> List.filterMap
                    (\(Node _ decl) ->
                        case decl of
                            FunctionDeclaration func ->
                                let
                                    (Node _ impl) =
                                        func.declaration
                                in
                                if Node.value impl.name == valueName then
                                    findDescribeListChildren impl.expression
                                        |> Maybe.map
                                            (List.map
                                                (\child ->
                                                    -- Qualify simple variable references with module name
                                                    -- so the wrapper can access unexposed functions
                                                    case Node.value child of
                                                        FunctionOrValue [] name ->
                                                            modName ++ "." ++ name

                                                        _ ->
                                                            extractSourceRange source child
                                                )
                                            )

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
                |> List.head


extractDescribeChildrenAsExpressions : String -> String -> Maybe (List (Node Expression))
extractDescribeChildrenAsExpressions valueName source =
    case Elm.Parser.parseToFile source of
        Err _ ->
            Nothing

        Ok file ->
            file.declarations
                |> List.filterMap
                    (\(Node _ decl) ->
                        case decl of
                            FunctionDeclaration func ->
                                let
                                    (Node _ impl) =
                                        func.declaration
                                in
                                if Node.value impl.name == valueName then
                                    findDescribeListChildren impl.expression

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
                |> List.head


{-| Find the list argument of a `describe "label" [children]` expression.
Handles both `describe "label" [...]` and `Test.describe "label" [...]`.
-}
findDescribeListChildren : Node Expression -> Maybe (List (Node Expression))
findDescribeListChildren (Node _ expr) =
    case expr of
        -- describe "label" [child1, child2, ...]
        Application nodes ->
            case nodes of
                [ Node _ (FunctionOrValue _ "describe"), _, Node _ (ListExpr children) ] ->
                    Just children

                [ Node _ (FunctionOrValue _ "describe"), _, listNode ] ->
                    -- The list might be wrapped in parens
                    findListExpr listNode

                _ ->
                    Nothing

        -- Could be wrapped in parens: (describe "label" [...])
        ParenthesizedExpression inner ->
            findDescribeListChildren inner

        _ ->
            Nothing


findListExpr : Node Expression -> Maybe (List (Node Expression))
findListExpr (Node _ expr) =
    case expr of
        ListExpr children ->
            Just children

        ParenthesizedExpression inner ->
            findListExpr inner

        _ ->
            Nothing


{-| Count how many test runners a child expression will produce, by
walking the AST. Returns `Nothing` when the expression involves patterns
we can't count statically (a helper function call, a bare reference,
anything we don't recognize), so the caller can fall back to the
coarse mode.

Used by `MutationTestRunner` for per-child precision on inline test
children — previously that fast path required children to be named
references (`Mod.namedTest`) so we could pass each child to
`SimpleTestRunner.countTests` through the interpreter. Most real-world
elm-test suites use inline `test "..." <| \() -> ...` shapes, so the
fast path never fired. Static counting fills that gap for the common
shapes without a round trip through the interpreter.

Handles:

  - `test label fn` → 1
  - `fuzz`, `fuzz2`, `fuzz3`, `fuzzWith` → 1
  - `todo label` → 1
  - `describe label [children]` → sum of child counts
  - `skip child` → same count as child
  - `only child` → same count as child
  - `concat [children]` → sum of child counts
  - `<|` and `|>` operator applications (normalized to plain `Application`)
  - parenthesized expressions

-}
countRunnersStatically : Node Expression -> Maybe Int
countRunnersStatically node =
    case normalizeTestExpr node of
        Node _ (Application ((Node _ (FunctionOrValue _ name)) :: args)) ->
            countByName name args

        _ ->
            Nothing


countByName : String -> List (Node Expression) -> Maybe Int
countByName name args =
    case name of
        "test" ->
            Just 1

        "fuzz" ->
            Just 1

        "fuzz2" ->
            Just 1

        "fuzz3" ->
            Just 1

        "fuzzWith" ->
            Just 1

        "todo" ->
            Just 1

        "describe" ->
            case args of
                [ _, listNode ] ->
                    findListExpr listNode
                        |> Maybe.andThen sumCounts

                _ ->
                    Nothing

        "concat" ->
            case args of
                [ listNode ] ->
                    findListExpr listNode
                        |> Maybe.andThen sumCounts

                _ ->
                    Nothing

        "skip" ->
            case args of
                [ child ] ->
                    countRunnersStatically child

                _ ->
                    Nothing

        "only" ->
            case args of
                [ child ] ->
                    countRunnersStatically child

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Normalize pipe/paren wrappers so the caller just needs to pattern-match
on `Application`. `f a b <| c` becomes `f a b c`; `c |> f a b` becomes
`f a b c`; `(x)` becomes `x`.
-}
normalizeTestExpr : Node Expression -> Node Expression
normalizeTestExpr (Node r e) =
    case e of
        ParenthesizedExpression inner ->
            normalizeTestExpr inner

        OperatorApplication "<|" _ left right ->
            case normalizeTestExpr left of
                Node _ (Application nodes) ->
                    normalizeTestExpr (Node r (Application (nodes ++ [ right ])))

                normalizedLeft ->
                    Node r (Application [ normalizedLeft, right ])

        OperatorApplication "|>" _ left right ->
            case normalizeTestExpr right of
                Node _ (Application nodes) ->
                    normalizeTestExpr (Node r (Application (nodes ++ [ left ])))

                normalizedRight ->
                    Node r (Application [ normalizedRight, left ])

        _ ->
            Node r e


{-| Map `countRunnersStatically` over a list, summing counts; short-circuit
to `Nothing` as soon as any child isn't statically countable.
-}
sumCounts : List (Node Expression) -> Maybe Int
sumCounts children =
    List.foldl
        (\child acc ->
            case acc of
                Nothing ->
                    Nothing

                Just total ->
                    countRunnersStatically child
                        |> Maybe.map ((+) total)
        )
        (Just 0)
        children


{-| Extract the source text for an AST node using its Range.
Uses 1-based rows and 1-based columns (matching Elm syntax Range).
-}
extractSourceRange : String -> Node Expression -> String
extractSourceRange source (Node range _) =
    let
        lines =
            String.lines source

        -- Extract lines within the range (1-based to 0-based)
        relevantLines =
            lines
                |> List.drop (range.start.row - 1)
                |> List.take (range.end.row - range.start.row + 1)
    in
    case relevantLines of
        [] ->
            ""

        [ single ] ->
            String.slice (range.start.column - 1) (range.end.column - 1) single

        first :: rest ->
            let
                firstPart =
                    String.dropLeft (range.start.column - 1) first

                lastLine =
                    List.reverse rest |> List.head |> Maybe.withDefault ""

                lastPart =
                    String.left (range.end.column - 1) lastLine

                middleLines =
                    rest |> List.reverse |> List.drop 1 |> List.reverse
            in
            String.join "\n" (firstPart :: middleLines ++ [ lastPart ])
