module SemanticHash exposing (DeclarationIndex, FileAspectHashes, ImportResolver, RawIndex, affectedRunnerIndices, buildImportResolver, buildIndexFromFile, buildIndexFromSource, buildMultiModuleIndex, buildMultiModuleIndexWithPackages, buildRawIndex, computeAspectHashesFromFile, computeAspectHashesFromSource, declarationHash, depsForExpression, diffIndices, emptyImportResolver, extractDependencies, extractDependenciesWithRanges, getSemanticHash, hashExpression, replaceModuleInRawIndex, resolveRawIndex, resolveRawIndexIncremental, semanticHashForEntry, transitiveDepsOf)

{-| Unison-style semantic hashing for Elm declarations.

Each function is hashed based on its AST structure + the semantic hashes
of all functions it calls. This creates a Merkle tree where changes
propagate only through actual dependency chains.

-}

import Dict exposing (Dict)
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import FNV1a
import Set exposing (Set)


{-| Sub-AST hashes for fine-grained cache invalidation.
Each field hashes a different aspect of a file's AST.
-}
type alias FileAspectHashes =
    { expressionsHash : String
    , declNamesHash : String
    , importsHash : String
    , exposingHash : String
    , customTypesHash : String
    , fullHash : String
    }


{-| Compute aspect hashes from source code.
-}
computeAspectHashesFromSource : String -> FileAspectHashes
computeAspectHashesFromSource source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            computeAspectHashesFromFile file

        Err _ ->
            { expressionsHash = ""
            , declNamesHash = ""
            , importsHash = ""
            , exposingHash = ""
            , customTypesHash = ""
            , fullHash = ""
            }


computeAspectHashesFromFile : File -> FileAspectHashes
computeAspectHashesFromFile file =
    let
        expressionsHash =
            file.declarations
                |> List.filterMap
                    (\(Node _ decl) ->
                        case decl of
                            FunctionDeclaration func ->
                                Just (hashExpression (Node.value func.declaration |> .expression))

                            _ ->
                                Nothing
                    )
                |> String.join "|"
                |> FNV1a.hash
                |> String.fromInt

        declNamesHash =
            file.declarations
                |> List.map
                    (\(Node _ decl) ->
                        case decl of
                            FunctionDeclaration func ->
                                let
                                    name =
                                        Node.value (Node.value func.declaration).name

                                    hasSignature =
                                        case func.signature of
                                            Just _ ->
                                                "T"

                                            Nothing ->
                                                "F"
                                in
                                name ++ ":" ++ hasSignature

                            AliasDeclaration alias_ ->
                                "alias:" ++ Node.value alias_.name

                            CustomTypeDeclaration type_ ->
                                "type:" ++ Node.value type_.name

                            PortDeclaration port_ ->
                                "port:" ++ Node.value port_.name

                            InfixDeclaration _ ->
                                "infix"

                            Destructuring _ _ ->
                                "destructuring"
                    )
                |> String.join ","
                |> FNV1a.hash
                |> String.fromInt

        importsHash =
            file.imports
                |> List.map
                    (\(Node _ imp) ->
                        let
                            modName =
                                Node.value imp.moduleName |> String.join "."

                            alias =
                                imp.moduleAlias
                                    |> Maybe.map (Node.value >> String.join ".")
                                    |> Maybe.withDefault ""

                            exposing_ =
                                case imp.exposingList of
                                    Just (Node _ (All _)) ->
                                        "(..)"

                                    Just (Node _ (Explicit exposes)) ->
                                        "("
                                            ++ (exposes
                                                    |> List.map
                                                        (\(Node _ e) ->
                                                            case e of
                                                                InfixExpose s ->
                                                                    s

                                                                FunctionExpose s ->
                                                                    s

                                                                TypeOrAliasExpose s ->
                                                                    s

                                                                TypeExpose { name } ->
                                                                    name
                                                        )
                                                    |> String.join ","
                                               )
                                            ++ ")"

                                    Nothing ->
                                        ""
                        in
                        modName ++ "|" ++ alias ++ "|" ++ exposing_
                    )
                |> String.join "\n"
                |> FNV1a.hash
                |> String.fromInt

        exposingHash =
            case Node.value file.moduleDefinition of
                NormalModule { exposingList } ->
                    hashExposingList exposingList

                PortModule { exposingList } ->
                    hashExposingList exposingList

                EffectModule { exposingList } ->
                    hashExposingList exposingList

        customTypesHash =
            file.declarations
                |> List.filterMap
                    (\(Node _ decl) ->
                        case decl of
                            CustomTypeDeclaration type_ ->
                                Just
                                    (Node.value type_.name
                                        ++ "="
                                        ++ (type_.constructors
                                                |> List.map
                                                    (\(Node _ ctor) ->
                                                        Node.value ctor.name
                                                            ++ "("
                                                            ++ String.fromInt (List.length ctor.arguments)
                                                            ++ ")"
                                                    )
                                                |> String.join "|"
                                           )
                                    )

                            _ ->
                                Nothing
                    )
                |> String.join ","
                |> FNV1a.hash
                |> String.fromInt

        fullHash =
            [ expressionsHash, declNamesHash, importsHash, exposingHash, customTypesHash ]
                |> String.join "|"
                |> FNV1a.hash
                |> String.fromInt
    in
    { expressionsHash = expressionsHash
    , declNamesHash = declNamesHash
    , importsHash = importsHash
    , exposingHash = exposingHash
    , customTypesHash = customTypesHash
    , fullHash = fullHash
    }


hashExposingList : Node Exposing -> String
hashExposingList (Node _ exposing_) =
    (case exposing_ of
        All _ ->
            "(..)"

        Explicit exposes ->
            exposes
                |> List.map
                    (\(Node _ e) ->
                        case e of
                            InfixExpose s ->
                                s

                            FunctionExpose s ->
                                s

                            TypeOrAliasExpose s ->
                                s

                            TypeExpose { name } ->
                                name
                    )
                |> List.sort
                |> String.join ","
    )
        |> FNV1a.hash
        |> String.fromInt


type alias DeclarationIndex =
    Dict String DeclarationInfo


type alias DeclarationInfo =
    { astHash : String
    , dependencies : Set String
    , semanticHash : String
    }


{-| Raw index: AST hashes and direct dependencies before Merkle resolution.
Cheaper to build than full DeclarationIndex — can be updated incrementally
by replacing one module's entries.
-}
type alias RawIndex =
    Dict String { astHash : String, deps : Set String }


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
                                    let
                                        impl =
                                            Node.value func.declaration
                                    in
                                    -- Include the binding name and argument
                                    -- patterns, not just the body. Otherwise a
                                    -- rename or arg-reorder mutation inside a
                                    -- let would hash identically to baseline
                                    -- and be falsely reported as equivalent.
                                    "F"
                                        ++ Node.value impl.name
                                        ++ "("
                                        ++ String.join "," (List.map hashPattern impl.arguments)
                                        ++ ")="
                                        ++ hashExpression impl.expression

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
extractDependencies expressionNode =
    extractDependenciesWithRanges expressionNode
        |> List.map (\( _, moduleName, name ) -> ( moduleName, name ))


extractDependenciesWithRanges : Node Expression -> List ( Range, List String, String )
extractDependenciesWithRanges ((Node range expr) as expressionNode) =
    case expr of
        FunctionOrValue moduleName name ->
            -- Include all references — the index builder will filter locals
            if isUppercase name then
                -- Constructor, not a function dependency
                []

            else
                [ ( range, moduleName, name ) ]

        OperatorApplication _ _ left right ->
            extractDependenciesWithRanges left ++ extractDependenciesWithRanges right

        Application exprs ->
            List.concatMap extractDependenciesWithRanges exprs

        IfBlock cond thenExpr elseExpr ->
            extractDependenciesWithRanges cond ++ extractDependenciesWithRanges thenExpr ++ extractDependenciesWithRanges elseExpr

        Negation inner ->
            extractDependenciesWithRanges inner

        TupledExpression exprs ->
            List.concatMap extractDependenciesWithRanges exprs

        ParenthesizedExpression inner ->
            extractDependenciesWithRanges inner

        LetExpression { declarations, expression } ->
            let
                declDeps =
                    declarations
                        |> List.concatMap
                            (\(Node _ letDecl) ->
                                case letDecl of
                                    LetFunction func ->
                                        extractDependenciesWithRanges (Node.value func.declaration |> .expression)

                                    LetDestructuring _ expr_ ->
                                        extractDependenciesWithRanges expr_
                            )
            in
            declDeps ++ extractDependenciesWithRanges expression

        CaseExpression { expression, cases } ->
            extractDependenciesWithRanges expression
                ++ List.concatMap (\( _, caseExpr ) -> extractDependenciesWithRanges caseExpr) cases

        LambdaExpression { expression } ->
            extractDependenciesWithRanges expression

        RecordExpr setters ->
            List.concatMap (\(Node _ ( _, valExpr )) -> extractDependenciesWithRanges valExpr) setters

        RecordUpdateExpression (Node recordNameRange recordName) setters ->
            -- `{ foo | a = 1 }` reads `foo`, so the updated record is
            -- a real dependency — without this, a mutation to `foo`'s
            -- definition wouldn't merkle-propagate to decls that
            -- update `foo`, and we'd silently report those mutations
            -- as equivalent.
            ( recordNameRange, [], recordName )
                :: List.concatMap (\(Node _ ( _, valExpr )) -> extractDependenciesWithRanges valExpr) setters

        ListExpr items ->
            List.concatMap extractDependenciesWithRanges items

        RecordAccess inner _ ->
            extractDependenciesWithRanges inner

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


{-| Compute a cache-key hash for a specific entry-point declaration.

This returns the declaration's semantic hash, which by the Merkle property
already transitively includes the hashes of all functions it calls.
Changes to unrelated functions (not in the transitive closure) don't
affect this hash.

This is the function to use as a cache key: if this hash hasn't changed,
the evaluation result is provably unchanged (Elm's purity guarantees this).

-}
semanticHashForEntry : DeclarationIndex -> String -> Maybe String
semanticHashForEntry index entryPoint =
    getSemanticHash index entryPoint


{-| Look up a declaration's semantic hash by module name and function name.

    declarationHash [ "Elm", "Core" ] "map" index
    --> Just "<hash>"

Preferred API for callers holding `ModuleName` (`List String`) — the form
used throughout elm-interpreter and elm-syntax — rather than the joined
internal key. Mirrors the multi-module key convention: joined module name
`++ "." ++` function name.

For single-module indices from `buildIndexFromSource` / `buildIndexFromFile`,
pass `[]` as the module name — those index by bare function name.

-}
declarationHash : List String -> String -> DeclarationIndex -> Maybe String
declarationHash moduleName funcName index =
    let
        key : String
        key =
            if List.isEmpty moduleName then
                funcName

            else
                String.join "." moduleName ++ "." ++ funcName
    in
    getSemanticHash index key


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


{-| Per-module view of `import` declarations, used to resolve qualified
references in expressions back to their real declaring module.

  - `aliasMap` maps the prefix a user writes (`Bar` in `Bar.baz`) to the real
    module name (`Foo.Bar`). Plain `import Foo` registers `"Foo" → "Foo"`;
    `import Foo.Bar as Bar` registers `"Bar" → "Foo.Bar"`. When a later
    `import X.Y as X` shadows an earlier `import X`, the later entry wins.
  - `exposedMap` maps an explicitly-exposed unqualified name (`bar` from
    `import Foo exposing (bar)`) to the real module it came from.
  - `openImports` lists modules imported with `exposing (..)`. For bare
    references, we try each in order and take the first match that exists in
    the index.

-}
type alias ImportResolver =
    { aliasMap : Dict String String
    , exposedMap : Dict String String
    , openImports : List String
    }


buildImportResolver : List (Node Import) -> ImportResolver
buildImportResolver imports =
    List.foldl
        (\(Node _ imp) acc ->
            let
                realModule : String
                realModule =
                    String.join "." (Node.value imp.moduleName)

                prefix : String
                prefix =
                    case imp.moduleAlias of
                        Just aliasNode ->
                            String.join "." (Node.value aliasNode)

                        Nothing ->
                            realModule

                withAlias : ImportResolver
                withAlias =
                    { acc | aliasMap = Dict.insert prefix realModule acc.aliasMap }
            in
            case imp.exposingList of
                Nothing ->
                    withAlias

                Just (Node _ (All _)) ->
                    { withAlias | openImports = withAlias.openImports ++ [ realModule ] }

                Just (Node _ (Explicit exposedList)) ->
                    List.foldl
                        (\(Node _ topLevel) resolver ->
                            case topLevel of
                                FunctionExpose name ->
                                    { resolver | exposedMap = Dict.insert name realModule resolver.exposedMap }

                                _ ->
                                    resolver
                        )
                        withAlias
                        exposedList
        )
        { aliasMap = Dict.empty, exposedMap = Dict.empty, openImports = [] }
        imports


{-| Resolve a `(modulePrefix, funcName)` reference from an expression AST to
a fully-qualified declaration name, consulting the module's import resolver.
Falls back to the historical behavior (prefix used as-is, bare names assumed
local to the current module) when no import matches.
-}
resolveImport : ImportResolver -> Set String -> String -> ( List String, String ) -> String
resolveImport resolver allDeclNames currentModule ( modName, funcName ) =
    case modName of
        [] ->
            case Dict.get funcName resolver.exposedMap of
                Just realModule ->
                    realModule ++ "." ++ funcName

                Nothing ->
                    let
                        fromOpen : Maybe String
                        fromOpen =
                            resolver.openImports
                                |> List.filterMap
                                    (\realModule ->
                                        let
                                            qualRef =
                                                realModule ++ "." ++ funcName
                                        in
                                        if Set.member qualRef allDeclNames then
                                            Just qualRef

                                        else
                                            Nothing
                                    )
                                |> List.head
                    in
                    fromOpen
                        |> Maybe.withDefault (currentModule ++ "." ++ funcName)

        _ ->
            let
                prefix : String
                prefix =
                    String.join "." modName
            in
            case Dict.get prefix resolver.aliasMap of
                Just realModule ->
                    realModule ++ "." ++ funcName

                Nothing ->
                    prefix ++ "." ++ funcName


{-| Build a raw index (AST hashes + direct deps) from module sources.
This is the expensive parsing step — do it once for the baseline.
-}
buildRawIndex : List { moduleName : String, source : String } -> RawIndex
buildRawIndex modules =
    let
        allDeclarations : List ( String, { expr : Node Expression, moduleName : String, resolver : ImportResolver } )
        allDeclarations =
            modules
                |> List.concatMap
                    (\mod ->
                        case Elm.Parser.parseToFile mod.source of
                            Ok file ->
                                let
                                    resolver : ImportResolver
                                    resolver =
                                        buildImportResolver file.imports
                                in
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
                                                        , { expr = impl.expression
                                                          , moduleName = mod.moduleName
                                                          , resolver = resolver
                                                          }
                                                        )

                                                _ ->
                                                    Nothing
                                        )

                            Err _ ->
                                []
                    )

        allDeclNames =
            allDeclarations |> List.map Tuple.first |> Set.fromList
    in
    allDeclarations
        |> List.map
            (\( qualName, { expr, moduleName, resolver } ) ->
                ( qualName
                , { astHash = hashExpression expr
                  , deps =
                        extractDependencies expr
                            |> List.filterMap
                                (\( modName, funcName ) ->
                                    if isUppercase funcName then
                                        Nothing

                                    else
                                        let
                                            qualRef =
                                                resolveImport resolver allDeclNames moduleName ( modName, funcName )
                                        in
                                        if Set.member qualRef allDeclNames && qualRef /= qualName then
                                            Just qualRef

                                        else
                                            Nothing
                                )
                            |> Set.fromList
                  }
                )
            )
        |> Dict.fromList


{-| Replace one module's entries in a raw index. Only reparses the single
mutated module source — O(1 module) instead of O(all modules).
-}
replaceModuleInRawIndex : RawIndex -> { moduleName : String, source : String } -> RawIndex
replaceModuleInRawIndex baseRawIndex mutatedModule =
    let
        prefix =
            mutatedModule.moduleName ++ "."

        -- Remove old entries for this module
        withoutOld =
            Dict.filter (\name _ -> not (String.startsWith prefix name)) baseRawIndex

        allDeclNames =
            Dict.keys baseRawIndex |> Set.fromList

        -- Parse mutated module and extract new entries
        newEntries =
            case Elm.Parser.parseToFile mutatedModule.source of
                Ok file ->
                    let
                        resolver : ImportResolver
                        resolver =
                            buildImportResolver file.imports
                    in
                    file.declarations
                        |> List.filterMap
                            (\(Node _ decl) ->
                                case decl of
                                    FunctionDeclaration func ->
                                        let
                                            impl =
                                                Node.value func.declaration

                                            qualName =
                                                mutatedModule.moduleName ++ "." ++ Node.value impl.name
                                        in
                                        Just
                                            ( qualName
                                            , { astHash = hashExpression impl.expression
                                              , deps =
                                                    extractDependencies impl.expression
                                                        |> List.filterMap
                                                            (\( modName, funcName ) ->
                                                                if isUppercase funcName then
                                                                    Nothing

                                                                else
                                                                    let
                                                                        qualRef =
                                                                            resolveImport resolver allDeclNames mutatedModule.moduleName ( modName, funcName )
                                                                    in
                                                                    if Set.member qualRef allDeclNames && qualRef /= qualName then
                                                                        Just qualRef

                                                                    else
                                                                        Nothing
                                                            )
                                                        |> Set.fromList
                                              }
                                            )

                                    _ ->
                                        Nothing
                            )

                Err _ ->
                    []
    in
    List.foldl (\( name, info ) idx -> Dict.insert name info idx) withoutOld newEntries


{-| Resolve a raw index into a full DeclarationIndex with Merkle semantic hashes.
-}
resolveRawIndex : RawIndex -> DeclarationIndex
resolveRawIndex rawIndex =
    let
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


{-| Incrementally compute a DeclarationIndex from a mutated raw index,
reusing baseline semantic hashes for unchanged declarations. Only re-resolves
hashes for declarations whose AST hash or dependencies changed.
Much faster than full resolveRawIndex when only one module is mutated.
-}
resolveRawIndexIncremental : DeclarationIndex -> RawIndex -> DeclarationIndex
resolveRawIndexIncremental baseline mutatedRaw =
    let
        -- Find declarations with changed AST hashes
        changedAstDecls =
            Dict.foldl
                (\name info acc ->
                    case Dict.get name baseline of
                        Just baseInfo ->
                            if baseInfo.astHash /= info.astHash then
                                Set.insert name acc

                            else
                                acc

                        Nothing ->
                            -- New declaration
                            Set.insert name acc
                )
                Set.empty
                mutatedRaw

        -- Build reverse dependency map
        reverseDeps =
            Dict.foldl
                (\name info acc ->
                    Set.foldl
                        (\dep revAcc ->
                            Dict.update dep
                                (\existing ->
                                    case existing of
                                        Just set ->
                                            Just (Set.insert name set)

                                        Nothing ->
                                            Just (Set.singleton name)
                                )
                                revAcc
                        )
                        acc
                        info.deps
                )
                Dict.empty
                mutatedRaw

        -- Propagate: find all transitively affected declarations
        affected =
            propagateChanges reverseDeps (Set.toList changedAstDecls) changedAstDecls

        -- Re-resolve only affected declarations
        semanticHashes =
            resolveAll mutatedRaw Dict.empty
    in
    mutatedRaw
        |> Dict.map
            (\name info ->
                if Set.member name affected then
                    { astHash = info.astHash
                    , dependencies = info.deps
                    , semanticHash =
                        Dict.get name semanticHashes
                            |> Maybe.withDefault info.astHash
                    }

                else
                    -- Unchanged: reuse baseline
                    case Dict.get name baseline of
                        Just baseInfo ->
                            baseInfo

                        Nothing ->
                            { astHash = info.astHash
                            , dependencies = info.deps
                            , semanticHash =
                                Dict.get name semanticHashes
                                    |> Maybe.withDefault info.astHash
                            }
            )


propagateChanges : Dict String (Set String) -> List String -> Set String -> Set String
propagateChanges reverseDeps queue visited =
    case queue of
        [] ->
            visited

        current :: rest ->
            let
                dependents =
                    Dict.get current reverseDeps
                        |> Maybe.withDefault Set.empty

                newDeps =
                    Set.diff dependents visited
            in
            propagateChanges reverseDeps (rest ++ Set.toList newDeps) (Set.union visited newDeps)


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


{-| Compare two DeclarationIndices and identify which declarations have
different semantic hashes. Thanks to the Merkle property, a declaration's
hash changes if ANY of its transitive dependencies changed — so the returned
`changed` set automatically includes reverse dependents.

This is the Salsa-style invalidation primitive: given the original and mutated
indices, the `changed` set tells you exactly which declarations need re-evaluation.
Everything in `unchanged` is provably identical (Elm purity guarantee).

-}
diffIndices : DeclarationIndex -> DeclarationIndex -> { changed : Set String, unchanged : Set String }
diffIndices original mutated =
    Dict.foldl
        (\name info acc ->
            case Dict.get name original of
                Just origInfo ->
                    if origInfo.semanticHash == info.semanticHash then
                        { acc | unchanged = Set.insert name acc.unchanged }

                    else
                        { acc | changed = Set.insert name acc.changed }

                Nothing ->
                    -- New declaration (not in original)
                    { acc | changed = Set.insert name acc.changed }
        )
        { changed = Set.empty, unchanged = Set.empty }
        mutated


{-| Given per-runner dependency sets and a set of changed declarations,
return the indices of runners whose dependencies intersect with the changes.

By Elm's purity: if a runner's transitive dependencies don't include any
changed declaration, its output is provably identical to baseline.

-}
affectedRunnerIndices : List (Set String) -> Set String -> List Int
affectedRunnerIndices perRunnerDeps changedDecls =
    perRunnerDeps
        |> List.indexedMap
            (\i deps ->
                if Set.isEmpty (Set.intersect deps changedDecls) then
                    Nothing

                else
                    Just i
            )
        |> List.filterMap identity


{-| Compute the transitive dependency closure for a declaration.
Returns all declarations reachable from the given name (including itself).
-}
transitiveDepsOf : DeclarationIndex -> String -> Set String
transitiveDepsOf index name =
    transitiveDepsHelp index [ name ] (Set.singleton name)


transitiveDepsHelp : DeclarationIndex -> List String -> Set String -> Set String
transitiveDepsHelp index queue visited =
    case queue of
        [] ->
            visited

        current :: rest ->
            let
                directDeps =
                    Dict.get current index
                        |> Maybe.map .dependencies
                        |> Maybe.withDefault Set.empty

                newDeps =
                    Set.diff directDeps visited

                newQueue =
                    rest ++ Set.toList newDeps
            in
            transitiveDepsHelp index newQueue (Set.union visited newDeps)


{-| Compute dependency set for an expression AST node, resolved against
the semantic index. Returns qualified names of all declarations the
expression transitively depends on.

Takes an `ImportResolver` for the module the expression came from so
aliased and re-exposed imports resolve to their real module name.
Without the resolver, `Bar.baz` from `import Foo.Bar as Bar` would
stay as `Bar.baz`, fail the `Dict.member` lookup, and silently drop
the dep — exactly the kind of silent false-NoCoverage the rest of
the codebase's alias handling was added to prevent.

Pass `emptyImportResolver` if the expression has no imports to honour.

-}
depsForExpression : DeclarationIndex -> ImportResolver -> String -> Node Expression -> Set String
depsForExpression index resolver moduleName expr =
    let
        allDeclNames : Set String
        allDeclNames =
            Dict.keys index |> Set.fromList

        directRefs =
            extractDependencies expr
                |> List.filterMap
                    (\( modName, funcName ) ->
                        if isUppercase funcName then
                            Nothing

                        else
                            let
                                qualRef =
                                    resolveImport resolver allDeclNames moduleName ( modName, funcName )
                            in
                            if Dict.member qualRef index then
                                Just qualRef

                            else
                                Nothing
                    )
                |> Set.fromList
    in
    -- Expand to transitive closure
    directRefs
        |> Set.toList
        |> List.foldl (\ref acc -> Set.union acc (transitiveDepsOf index ref)) directRefs


{-| Empty resolver for the "no imports to worry about" case (e.g. the
expression lives in the same module as the callers it references).
-}
emptyImportResolver : ImportResolver
emptyImportResolver =
    { aliasMap = Dict.empty
    , exposedMap = Dict.empty
    , openImports = []
    }
