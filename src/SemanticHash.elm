module SemanticHash exposing (DeclarationIndex, buildIndexFromSource, buildMultiModuleIndex, buildMultiModuleIndexWithPackages, extractDependencies, getSemanticHash, hashExpression)

{-| Unison-style semantic hashing for Elm declarations.

Each function is hashed based on its AST structure + the semantic hashes
of all functions it calls. This creates a Merkle tree where changes
propagate only through actual dependency chains.

-}

import Dict exposing (Dict)
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Set exposing (Set)


type alias DeclarationIndex =
    Dict String DeclarationInfo


type alias DeclarationInfo =
    { astHash : String
    , dependencies : Set String
    , semanticHash : String
    }


{-| Hash an expression's AST structure. References to other functions
are included by their qualified name (not resolved to semantic hashes —
that's done at the index level).
-}
hashExpression : Node Expression -> String
hashExpression (Node _ expr) =
    case expr of
        Integer n ->
            "I" ++ String.fromInt n

        Floatable f ->
            "F" ++ String.fromFloat f

        Literal s ->
            "S\"" ++ s ++ "\""

        CharLiteral c ->
            "C" ++ String.fromChar c

        Hex n ->
            "H" ++ String.fromInt n

        UnitExpr ->
            "()"

        FunctionOrValue moduleName name ->
            "V" ++ String.join "." moduleName ++ "." ++ name

        PrefixOperator op ->
            "PO" ++ op

        Operator op ->
            "OP" ++ op

        OperatorApplication op _ left right ->
            "(" ++ hashExpression left ++ op ++ hashExpression right ++ ")"

        Application exprs ->
            "A(" ++ String.join " " (List.map hashExpression exprs) ++ ")"

        IfBlock cond thenExpr elseExpr ->
            "IF(" ++ hashExpression cond ++ "," ++ hashExpression thenExpr ++ "," ++ hashExpression elseExpr ++ ")"

        Negation inner ->
            "NEG(" ++ hashExpression inner ++ ")"

        TupledExpression exprs ->
            "T(" ++ String.join "," (List.map hashExpression exprs) ++ ")"

        ParenthesizedExpression inner ->
            hashExpression inner

        LetExpression { declarations, expression } ->
            "LET("
                ++ String.join ";"
                    (List.map
                        (\(Node _ letDecl) ->
                            case letDecl of
                                LetFunction func ->
                                    "F" ++ hashExpression (Node.value func.declaration |> .expression)

                                LetDestructuring pat expr_ ->
                                    "D" ++ hashPattern pat ++ "=" ++ hashExpression expr_
                        )
                        declarations
                    )
                ++ " IN "
                ++ hashExpression expression
                ++ ")"

        CaseExpression { expression, cases } ->
            "CASE("
                ++ hashExpression expression
                ++ ","
                ++ String.join ";"
                    (List.map
                        (\( pat, caseExpr ) ->
                            hashPattern pat ++ "->" ++ hashExpression caseExpr
                        )
                        cases
                    )
                ++ ")"

        LambdaExpression { args, expression } ->
            "L("
                ++ String.join "," (List.map hashPattern args)
                ++ "->"
                ++ hashExpression expression
                ++ ")"

        RecordExpr setters ->
            "R{"
                ++ String.join ","
                    (List.map
                        (\(Node _ ( Node _ name, valExpr )) ->
                            name ++ "=" ++ hashExpression valExpr
                        )
                        setters
                    )
                ++ "}"

        RecordUpdateExpression (Node _ name) setters ->
            "RU{"
                ++ name
                ++ "|"
                ++ String.join ","
                    (List.map
                        (\(Node _ ( Node _ fieldName, valExpr )) ->
                            fieldName ++ "=" ++ hashExpression valExpr
                        )
                        setters
                    )
                ++ "}"

        ListExpr items ->
            "[" ++ String.join "," (List.map hashExpression items) ++ "]"

        RecordAccess inner (Node _ field) ->
            hashExpression inner ++ "." ++ field

        RecordAccessFunction field ->
            "." ++ field

        GLSLExpression _ ->
            "GLSL"


{-| Hash a pattern structure for use in case/let/lambda hashing.
-}
hashPattern : Node Pattern -> String
hashPattern (Node _ pat) =
    case pat of
        AllPattern ->
            "_"

        UnitPattern ->
            "()"

        CharPattern c ->
            "'" ++ String.fromChar c ++ "'"

        StringPattern s ->
            "\"" ++ s ++ "\""

        IntPattern n ->
            String.fromInt n

        HexPattern n ->
            "0x" ++ String.fromInt n

        FloatPattern f ->
            String.fromFloat f

        TuplePattern pats ->
            "(" ++ String.join "," (List.map hashPattern pats) ++ ")"

        RecordPattern fields ->
            "{" ++ String.join "," (List.map Node.value fields) ++ "}"

        UnConsPattern head tail ->
            hashPattern head ++ "::" ++ hashPattern tail

        ListPattern items ->
            "[" ++ String.join "," (List.map hashPattern items) ++ "]"

        VarPattern name ->
            "v" ++ name

        NamedPattern { moduleName, name } args ->
            String.join "." moduleName ++ name ++ String.join " " (List.map hashPattern args)

        AsPattern inner (Node _ alias_) ->
            hashPattern inner ++ " as " ++ alias_

        ParenthesizedPattern inner ->
            hashPattern inner


{-| Extract all function/value references from an expression.
Returns (moduleName, functionName) pairs. Local variables (parameters,
let bindings) are included — the caller should filter them if needed.
-}
extractDependencies : Node Expression -> List ( List String, String )
extractDependencies (Node _ expr) =
    case expr of
        FunctionOrValue moduleName name ->
            -- Include all references — the index builder will filter locals
            if isUppercase name then
                -- Constructor, not a function dependency
                []

            else
                [ ( moduleName, name ) ]

        OperatorApplication _ _ left right ->
            extractDependencies left ++ extractDependencies right

        Application exprs ->
            List.concatMap extractDependencies exprs

        IfBlock cond thenExpr elseExpr ->
            extractDependencies cond ++ extractDependencies thenExpr ++ extractDependencies elseExpr

        Negation inner ->
            extractDependencies inner

        TupledExpression exprs ->
            List.concatMap extractDependencies exprs

        ParenthesizedExpression inner ->
            extractDependencies inner

        LetExpression { declarations, expression } ->
            let
                declDeps =
                    declarations
                        |> List.concatMap
                            (\(Node _ letDecl) ->
                                case letDecl of
                                    LetFunction func ->
                                        extractDependencies (Node.value func.declaration |> .expression)

                                    LetDestructuring _ expr_ ->
                                        extractDependencies expr_
                            )
            in
            declDeps ++ extractDependencies expression

        CaseExpression { expression, cases } ->
            extractDependencies expression
                ++ List.concatMap (\( _, caseExpr ) -> extractDependencies caseExpr) cases

        LambdaExpression { expression } ->
            extractDependencies expression

        RecordExpr setters ->
            List.concatMap (\(Node _ ( _, valExpr )) -> extractDependencies valExpr) setters

        RecordUpdateExpression _ setters ->
            List.concatMap (\(Node _ ( _, valExpr )) -> extractDependencies valExpr) setters

        ListExpr items ->
            List.concatMap extractDependencies items

        RecordAccess inner _ ->
            extractDependencies inner

        _ ->
            []


isUppercase : String -> Bool
isUppercase name =
    case String.uncons name of
        Just ( c, _ ) ->
            Char.isUpper c

        Nothing ->
            False


{-| Build a semantic hash index from a single module's source.
Computes per-declaration semantic hashes using Merkle-style hashing.
-}
buildIndexFromSource : String -> DeclarationIndex
buildIndexFromSource source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            buildIndexFromFile file

        Err _ ->
            Dict.empty


buildIndexFromFile : File -> DeclarationIndex
buildIndexFromFile file =
    let
        -- Extract all top-level function declarations
        declarations : List ( String, Node Expression )
        declarations =
            file.declarations
                |> List.filterMap
                    (\(Node _ decl) ->
                        case decl of
                            FunctionDeclaration func ->
                                let
                                    impl =
                                        Node.value func.declaration
                                in
                                Just ( Node.value impl.name, impl.expression )

                            _ ->
                                Nothing
                    )

        -- Build initial index with AST hashes and raw dependencies
        declNames : Set String
        declNames =
            declarations |> List.map Tuple.first |> Set.fromList

        rawIndex : Dict String { astHash : String, deps : Set String }
        rawIndex =
            declarations
                |> List.map
                    (\( name, expr ) ->
                        let
                            astHash =
                                hashExpression expr

                            deps =
                                extractDependencies expr
                                    |> List.filterMap
                                        (\( modName, funcName ) ->
                                            -- Only track dependencies on other declarations in this module
                                            if List.isEmpty modName && Set.member funcName declNames && funcName /= name then
                                                Just funcName

                                            else
                                                Nothing
                                        )
                                    |> Set.fromList
                        in
                        ( name, { astHash = astHash, deps = deps } )
                    )
                |> Dict.fromList

        semanticHashes : Dict String String
        semanticHashes =
            resolveAll rawIndex Dict.empty
    in
    -- Build final index
    rawIndex
        |> Dict.map
            (\name info ->
                { astHash = info.astHash
                , dependencies = info.deps
                , semanticHash =
                    Dict.get name semanticHashes
                        |> Maybe.withDefault info.astHash
                }
            )


{-| Look up a declaration's semantic hash in the index.
Accepts either "funcName" (single-module) or "ModuleName.funcName" (multi-module).
-}
getSemanticHash : DeclarationIndex -> String -> Maybe String
getSemanticHash index name =
    Dict.get name index
        |> Maybe.map .semanticHash


{-| Build a semantic hash index from multiple modules.
Cross-module references are resolved to their semantic hashes (Merkle property).
-}
buildMultiModuleIndex : List { moduleName : String, source : String } -> DeclarationIndex
buildMultiModuleIndex modules =
    buildMultiModuleIndexWithPackages { packageVersions = [], modules = modules }


{-| Build a semantic hash index with package version info.
Package references are hashed as `packageVersion.ModuleName.functionName`.
-}
buildMultiModuleIndexWithPackages :
    { packageVersions : List ( String, String )
    , modules : List { moduleName : String, source : String }
    }
    -> DeclarationIndex
buildMultiModuleIndexWithPackages { packageVersions, modules } =
    let
        packageVersionDict : Dict String String
        packageVersionDict =
            Dict.fromList packageVersions

        -- Parse all modules and extract declarations with qualified names
        allDeclarations : List ( String, { expr : Node Expression, moduleName : String } )
        allDeclarations =
            modules
                |> List.concatMap
                    (\mod ->
                        case Elm.Parser.parseToFile mod.source of
                            Ok file ->
                                file.declarations
                                    |> List.filterMap
                                        (\(Node _ decl) ->
                                            case decl of
                                                FunctionDeclaration func ->
                                                    let
                                                        impl =
                                                            Node.value func.declaration
                                                    in
                                                    Just
                                                        ( mod.moduleName ++ "." ++ Node.value impl.name
                                                        , { expr = impl.expression, moduleName = mod.moduleName }
                                                        )

                                                _ ->
                                                    Nothing
                                        )

                            Err _ ->
                                []
                    )

        -- All qualified declaration names
        allDeclNames : Set String
        allDeclNames =
            allDeclarations |> List.map Tuple.first |> Set.fromList

        -- Build raw index: AST hash + qualified dependencies
        rawIndex : Dict String { astHash : String, deps : Set String }
        rawIndex =
            allDeclarations
                |> List.map
                    (\( qualName, { expr, moduleName } ) ->
                        let
                            astHash =
                                hashExpressionWithPackages packageVersionDict expr

                            deps =
                                extractDependencies expr
                                    |> List.filterMap
                                        (\( modName, funcName ) ->
                                            if isUppercase funcName then
                                                Nothing

                                            else
                                                let
                                                    qualRef =
                                                        if List.isEmpty modName then
                                                            -- Unqualified: could be same module or local
                                                            moduleName ++ "." ++ funcName

                                                        else
                                                            String.join "." modName ++ "." ++ funcName
                                                in
                                                if Set.member qualRef allDeclNames && qualRef /= qualName then
                                                    Just qualRef

                                                else
                                                    Nothing
                                        )
                                    |> Set.fromList
                        in
                        ( qualName, { astHash = astHash, deps = deps } )
                    )
                |> Dict.fromList

        -- Topological resolution (same algorithm as single-module)
        semanticHashes : Dict String String
        semanticHashes =
            resolveAll rawIndex Dict.empty
    in
    rawIndex
        |> Dict.map
            (\name info ->
                { astHash = info.astHash
                , dependencies = info.deps
                , semanticHash =
                    Dict.get name semanticHashes
                        |> Maybe.withDefault info.astHash
                }
            )


{-| Hash an expression, replacing package references with versioned names.
-}
hashExpressionWithPackages : Dict String String -> Node Expression -> String
hashExpressionWithPackages packageVersions (Node _ expr) =
    case expr of
        FunctionOrValue moduleName name ->
            let
                moduleStr =
                    String.join "." moduleName
            in
            case Dict.get moduleStr packageVersions of
                Just version ->
                    "P" ++ version ++ "." ++ moduleStr ++ "." ++ name

                Nothing ->
                    "V" ++ moduleStr ++ "." ++ name

        OperatorApplication op _ left right ->
            "(" ++ hashExpressionWithPackages packageVersions left ++ op ++ hashExpressionWithPackages packageVersions right ++ ")"

        Application exprs ->
            "A(" ++ String.join " " (List.map (hashExpressionWithPackages packageVersions) exprs) ++ ")"

        IfBlock cond thenExpr elseExpr ->
            "IF(" ++ hashExpressionWithPackages packageVersions cond ++ "," ++ hashExpressionWithPackages packageVersions thenExpr ++ "," ++ hashExpressionWithPackages packageVersions elseExpr ++ ")"

        Negation inner ->
            "NEG(" ++ hashExpressionWithPackages packageVersions inner ++ ")"

        _ ->
            -- Fall back to base hashExpression for all other cases
            hashExpression (Node { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } } expr)


{-| Resolve semantic hashes iteratively in topological order.
Process declarations whose deps are all resolved first, then their dependents.
Handles cycles by falling back to AST-only hashes.
-}
resolveAll : Dict String { astHash : String, deps : Set String } -> Dict String String -> Dict String String
resolveAll remaining resolved =
    let
        ready =
            remaining
                |> Dict.filter
                    (\_ info ->
                        Set.toList info.deps
                            |> List.all (\dep -> Dict.member dep resolved)
                    )
    in
    if Dict.isEmpty ready then
        -- Either done or have a cycle — resolve remaining with just AST hash
        Dict.foldl
            (\name info acc -> Dict.insert name info.astHash acc)
            resolved
            remaining

    else
        let
            newResolved =
                Dict.foldl
                    (\name info acc ->
                        let
                            depHashes =
                                info.deps
                                    |> Set.toList
                                    |> List.sort
                                    |> List.filterMap (\dep -> Dict.get dep acc)

                            semanticHash =
                                info.astHash ++ "|" ++ String.join "|" depHashes
                        in
                        Dict.insert name semanticHash acc
                    )
                    resolved
                    ready

            newRemaining =
                Dict.filter (\name _ -> not (Dict.member name ready)) remaining
        in
        resolveAll newRemaining newResolved
