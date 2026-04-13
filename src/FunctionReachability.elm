module FunctionReachability exposing (Reference, computeReachable, findTopLevelNames, functionRefsPerModule)

{-| Function-level reachability analysis across a set of parsed user
modules.

Given a starting set of `(moduleName, functionName)` roots (typically
the exposed `Test`-typed values from test files), this walks each
root's body and collects all outbound function references — qualified
cross-module calls, alias-resolved calls, and same-module unqualified
calls. References are added to a work queue and the walk continues
until no new references are discovered.

The resulting `Set Reference` tells the interpreter's normalization
pass which 0-arg constants are worth pre-computing. For the
core-extra benchmark, this prunes `String.Diacritics.lookupArray`
(and its dependency `lookupTable`) from the live set when the test
workload doesn't transitively call `String.Extra.removeDiacritics`,
recovering the pre-regression cold-load cost.

Limitations:

  - `exposing (..)` imports are treated conservatively: we cannot
    enumerate every name without access to the imported module's
    file, so we skip unqualified resolution in that case. Qualified
    references still work.
  - Reflection / kernel dispatch isn't tracked. If a function is only
    reachable via a kernel's runtime lookup, it won't appear in the
    reachable set. In practice this is fine because kernels operate
    on values, not top-level constants.
  - The walker doesn't recurse into package modules — those aren't
    in `moduleFiles` anyway.

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression as Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Set exposing (Set)


{-| A fully-qualified function reference: `("String.Diacritics", "lookupArray")`.
-}
type alias Reference =
    ( String, String )


{-| BFS from `roots` through function bodies, returning the transitive
reachable set. Each pair in the output is `(moduleName, funcName)`
where `moduleName` is dot-joined (e.g. `"String.Diacritics"`).
-}
computeReachable : Dict String File -> Set Reference -> Set Reference
computeReachable moduleFiles roots =
    bfs moduleFiles roots roots


bfs : Dict String File -> Set Reference -> Set Reference -> Set Reference
bfs moduleFiles visited frontier =
    if Set.isEmpty frontier then
        visited

    else
        let
            newRefs : Set Reference
            newRefs =
                frontier
                    |> Set.foldl
                        (\ref acc -> Set.union acc (nextRefs moduleFiles ref))
                        Set.empty

            nextFrontier : Set Reference
            nextFrontier =
                Set.diff newRefs visited
        in
        bfs moduleFiles (Set.union visited newRefs) nextFrontier


{-| All outbound references reachable from one function's body.
-}
nextRefs : Dict String File -> Reference -> Set Reference
nextRefs moduleFiles ( moduleName, funcName ) =
    case Dict.get moduleName moduleFiles of
        Nothing ->
            Set.empty

        Just file ->
            case findFunctionBody funcName file of
                Nothing ->
                    Set.empty

                Just body ->
                    collectRefs (buildResolver file moduleName) body


findFunctionBody : String -> File -> Maybe (Node Expression)
findFunctionBody name file =
    file.declarations
        |> List.filterMap
            (\(Node _ decl) ->
                case decl of
                    FunctionDeclaration function ->
                        let
                            impl =
                                Node.value function.declaration
                        in
                        if Node.value impl.name == name then
                            Just impl.expression

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.head


{-| Collect every top-level function / value name defined in a file.
Handy for seeding reachability from a test module when you don't want
to enumerate specific `Test`-typed values.
-}
findTopLevelNames : File -> Set String
findTopLevelNames file =
    file.declarations
        |> List.filterMap
            (\(Node _ decl) ->
                case decl of
                    FunctionDeclaration function ->
                        let
                            impl =
                                Node.value function.declaration
                        in
                        Just (Node.value impl.name)

                    _ ->
                        Nothing
            )
        |> Set.fromList


{-| For each function defined in a file, return the set of outbound
references its body makes. Exposed for diagnostics / tests; the main
`computeReachable` uses this indirectly via `nextRefs`.
-}
functionRefsPerModule : File -> String -> Dict String (Set Reference)
functionRefsPerModule file moduleName =
    let
        resolver : Resolver
        resolver =
            buildResolver file moduleName
    in
    file.declarations
        |> List.filterMap
            (\(Node _ decl) ->
                case decl of
                    FunctionDeclaration function ->
                        let
                            impl =
                                Node.value function.declaration

                            name =
                                Node.value impl.name
                        in
                        Just ( name, collectRefs resolver impl.expression )

                    _ ->
                        Nothing
            )
        |> Dict.fromList


{-| A resolver turns a qualifier + name from a `FunctionOrValue` node
into a fully-qualified `Reference`, given the enclosing module's
imports and own definitions.
-}
type alias Resolver =
    List String -> String -> Maybe Reference


buildResolver : File -> String -> Resolver
buildResolver file currentModuleName =
    let
        aliases : Dict String String
        aliases =
            file.imports
                |> List.filterMap
                    (\(Node _ imp) ->
                        case imp.moduleAlias of
                            Just (Node _ aliasMn) ->
                                Just ( String.join "." aliasMn, String.join "." (Node.value imp.moduleName) )

                            Nothing ->
                                Nothing
                    )
                |> Dict.fromList

        exposedValues : Dict String String
        exposedValues =
            file.imports
                |> List.concatMap
                    (\(Node _ imp) ->
                        let
                            sourceModule =
                                String.join "." (Node.value imp.moduleName)
                        in
                        case imp.exposingList of
                            Nothing ->
                                []

                            Just (Node _ (All _)) ->
                                []

                            Just (Node _ (Explicit exposes)) ->
                                exposes
                                    |> List.filterMap
                                        (\(Node _ expose) ->
                                            case expose of
                                                FunctionExpose name ->
                                                    Just ( name, sourceModule )

                                                _ ->
                                                    Nothing
                                        )
                    )
                |> Dict.fromList

        definedInThisFile : Set String
        definedInThisFile =
            findTopLevelNames file
    in
    \qualifier name ->
        case qualifier of
            [] ->
                if Set.member name definedInThisFile then
                    Just ( currentModuleName, name )

                else
                    case Dict.get name exposedValues of
                        Just sourceModule ->
                            Just ( sourceModule, name )

                        Nothing ->
                            Nothing

            _ ->
                let
                    asString =
                        String.join "." qualifier
                in
                case Dict.get asString aliases of
                    Just realModule ->
                        Just ( realModule, name )

                    Nothing ->
                        Just ( asString, name )


{-| Walk an expression AST and return every resolved `FunctionOrValue`
reference. Operators, pattern bindings, let-bound locals, and lambda
parameters are not tracked — the walker simply collects refs without
worrying about shadowing, because our use case (reachability across
top-level constants) tolerates a small amount of over-approximation.
A local shadow that happens to match a real top-level name just keeps
that name in the live set, which is fine.
-}
collectRefs : Resolver -> Node Expression -> Set Reference
collectRefs resolve (Node _ expr) =
    case expr of
        FunctionOrValue qualifier name ->
            case resolve qualifier name of
                Just ref ->
                    Set.singleton ref

                Nothing ->
                    Set.empty

        Application children ->
            List.foldl (\e acc -> Set.union (collectRefs resolve e) acc) Set.empty children

        OperatorApplication _ _ l r ->
            Set.union (collectRefs resolve l) (collectRefs resolve r)

        IfBlock c t f ->
            Set.union (collectRefs resolve c) (Set.union (collectRefs resolve t) (collectRefs resolve f))

        Negation inner ->
            collectRefs resolve inner

        TupledExpression items ->
            List.foldl (\e acc -> Set.union (collectRefs resolve e) acc) Set.empty items

        ParenthesizedExpression inner ->
            collectRefs resolve inner

        LetExpression block ->
            letRefs resolve block

        CaseExpression { expression, cases } ->
            let
                exprRefs : Set Reference
                exprRefs =
                    collectRefs resolve expression

                caseRefs : Set Reference
                caseRefs =
                    List.foldl (\( _, body ) acc -> Set.union (collectRefs resolve body) acc) Set.empty cases
            in
            Set.union exprRefs caseRefs

        LambdaExpression { expression } ->
            collectRefs resolve expression

        RecordExpr fields ->
            List.foldl
                (\(Node _ ( _, value )) acc -> Set.union (collectRefs resolve value) acc)
                Set.empty
                fields

        ListExpr items ->
            List.foldl (\e acc -> Set.union (collectRefs resolve e) acc) Set.empty items

        RecordAccess record _ ->
            collectRefs resolve record

        RecordUpdateExpression _ setters ->
            List.foldl
                (\(Node _ ( _, value )) acc -> Set.union (collectRefs resolve value) acc)
                Set.empty
                setters

        _ ->
            Set.empty


letRefs : Resolver -> Expression.LetBlock -> Set Reference
letRefs resolve { declarations, expression } =
    let
        bodyRefs : Set Reference
        bodyRefs =
            collectRefs resolve expression

        declRefs : Set Reference
        declRefs =
            declarations
                |> List.foldl
                    (\(Node _ decl) acc ->
                        case decl of
                            LetFunction f ->
                                let
                                    impl =
                                        Node.value f.declaration
                                in
                                Set.union acc (collectRefs resolve impl.expression)

                            LetDestructuring _ val ->
                                Set.union acc (collectRefs resolve val)
                    )
                    Set.empty
    in
    Set.union bodyRefs declRefs
