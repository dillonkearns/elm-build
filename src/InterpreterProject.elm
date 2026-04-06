module InterpreterProject exposing (InterpreterProject, load, loadWith, eval, evalWith, evalWithCoverage, evalWithFileOverrides, evalWithSourceOverrides, getDepGraph, getPackageEnv, prepareAndEval, prepareAndEvalRaw, prepareAndEvalWithIntercepts, prepareAndEvalWithValues, prepareAndEvalWithYield, prepareAndEvalWithYieldState, prepareEvalSources)

{-| Evaluate and cache Elm expressions via the pure Elm interpreter.

Mirrors `ElmProject` structurally but replaces `elm make` + `node` with
`Cache.compute` + `Eval.Module.evalProject`.

-}

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Glob as Glob
import BackendTask.Time
import Cache exposing (FileOrDirectory)
import DepGraph
import Dict exposing (Dict)
import FastDict
import Coverage
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import SemanticHash
import Eval.Module
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Path exposing (Path)
import ProjectSources
import Set exposing (Set)
import Time
import Types


type alias ModuleGraph =
    { moduleToSource : Dict String String
    , moduleToFile : Dict String File
    , imports : Dict String (Set String)
    }


type InterpreterProject
    = InterpreterProject
        { sourceDirectories : List String
        , inputsByPath : Dict String ( Path, Cache.Monad FileOrDirectory )
        , userFileContents : Dict String String
        , depGraph : DepGraph.Graph
        , patchedPackageSources : List String
        , extraSources : List String
        , moduleGraph : ModuleGraph
        , packageModuleNames : Set String
        , packageEnv : Eval.Module.ProjectEnv
        , baseUserEnv : Maybe Eval.Module.ProjectEnv
        , semanticIndex : SemanticHash.DeclarationIndex
        }


{-| Initialize an InterpreterProject with default settings.

Reads elm.json for source directories, loads all package dependencies,
and sets up the project for interpreter evaluation.

-}
load :
    { projectDir : Path }
    -> BackendTask FatalError InterpreterProject
load { projectDir } =
    loadWith
        { projectDir = projectDir
        , skipPackages = Set.empty
        , patchSource = identity
        , extraSourceFiles = []
        , sourceDirectories = Nothing
        }


{-| Initialize an InterpreterProject with advanced options.

  - `skipPackages` — package names to exclude (e.g. those with kernel code)
  - `patchSource` — transform applied to each package source after loading
  - `extraSourceFiles` — additional source files to include
  - `sourceDirectories` — `Nothing` reads from elm.json, `Just` overrides

-}
loadWith :
    { projectDir : Path
    , skipPackages : Set String
    , patchSource : String -> String
    , extraSourceFiles : List String
    , sourceDirectories : Maybe (List String)
    }
    -> BackendTask FatalError InterpreterProject
loadWith config =
    -- Resolve source directories from config or elm.json
    (case config.sourceDirectories of
        Just dirs ->
            BackendTask.succeed dirs

        Nothing ->
            readSourceDirectories config.projectDir
    )
        |> BackendTask.andThen
            (\sourceDirectories ->
                -- Load and patch package sources
                ProjectSources.loadPackageDepsCached
                    { projectDir = config.projectDir
                    , skipPackages = config.skipPackages
                    }
                    |> BackendTask.andThen
                        (\packageSources ->
                            let
                                patchedPackageSources : List String
                                patchedPackageSources =
                                    List.map config.patchSource packageSources

                                -- Derive user source globs from source directories
                                userSourceGlobs : List String
                                userSourceGlobs =
                                    sourceDirectories
                                        |> List.map (\dir -> dir ++ "/**/*.elm")
                            in
                            -- Glob all user source globs for .elm files
                            userSourceGlobs
                                |> List.map
                                    (\globPattern ->
                                        Glob.fromStringWithOptions
                                            (let
                                                o : Glob.Options
                                                o =
                                                    Glob.defaultOptions
                                             in
                                             { o | include = Glob.OnlyFiles }
                                            )
                                            globPattern
                                    )
                                |> BackendTask.Extra.combine
                                |> BackendTask.map (List.concat >> List.sort)
                                |> BackendTask.andThen
                                    (\elmFiles ->
                                        -- Read each user source file's content
                                        elmFiles
                                            |> List.map
                                                (\filePath ->
                                                    File.rawFile filePath
                                                        |> BackendTask.allowFatal
                                                        |> BackendTask.map (\content -> ( filePath, content ))
                                                )
                                            |> BackendTask.Extra.combine
                                            |> BackendTask.andThen
                                                (\userFileContents ->
                                                    -- Read extra source files
                                                    config.extraSourceFiles
                                                        |> List.map
                                                            (\filePath ->
                                                                File.rawFile filePath
                                                                    |> BackendTask.allowFatal
                                                                    |> BackendTask.map (\content -> ( filePath, content ))
                                                            )
                                                        |> BackendTask.Extra.combine
                                                        |> BackendTask.andThen
                                                            (\extraFileContents ->
                                                                let
                                                                    depGraph : DepGraph.Graph
                                                                    depGraph =
                                                                        DepGraph.buildGraph
                                                                            { sourceDirectories = sourceDirectories
                                                                            , files =
                                                                                userFileContents
                                                                                    |> List.map
                                                                                        (\( filePath, content ) ->
                                                                                            { filePath = filePath
                                                                                            , content = content
                                                                                            }
                                                                                        )
                                                                            }

                                                                    allUserPaths : List Path
                                                                    allUserPaths =
                                                                        elmFiles
                                                                            |> List.map Path.path

                                                                    allSourceStrings : List String
                                                                    allSourceStrings =
                                                                        patchedPackageSources
                                                                            ++ List.map Tuple.second extraFileContents
                                                                            ++ List.map Tuple.second userFileContents

                                                                    allModules : List ( String, String )
                                                                    allModules =
                                                                        List.filterMap
                                                                            (\src ->
                                                                                DepGraph.parseModuleName src
                                                                                    |> Maybe.map (\name -> ( name, src ))
                                                                            )
                                                                            allSourceStrings

                                                                    -- Pre-parse user source files for reuse across evaluations
                                                                    userParsedFiles : Dict String File
                                                                    userParsedFiles =
                                                                        userFileContents
                                                                            |> List.filterMap
                                                                                (\( _, content ) ->
                                                                                    case Elm.Parser.parseToFile content of
                                                                                        Ok file ->
                                                                                            DepGraph.parseModuleName content
                                                                                                |> Maybe.map (\name -> ( name, file ))

                                                                                        Err _ ->
                                                                                            Nothing
                                                                                )
                                                                            |> Dict.fromList

                                                                    moduleGraph : ModuleGraph
                                                                    moduleGraph =
                                                                        { moduleToSource = Dict.fromList allModules
                                                                        , moduleToFile = userParsedFiles
                                                                        , imports =
                                                                            allModules
                                                                                |> List.map
                                                                                    (\( name, src ) ->
                                                                                        ( name, DepGraph.parseImports src |> Set.fromList )
                                                                                    )
                                                                                |> Dict.fromList
                                                                        }

                                                                    allStableSources : List String
                                                                    allStableSources =
                                                                        patchedPackageSources ++ List.map Tuple.second extraFileContents

                                                                    pkgModuleNames : Set String
                                                                    pkgModuleNames =
                                                                        allStableSources
                                                                            |> List.filterMap DepGraph.parseModuleName
                                                                            |> Set.fromList

                                                                    -- Include ALL package sources in the env (not just reachable from user code).
                                                                    -- This ensures that source overrides added at eval time (e.g. SimpleTestRunner)
                                                                    -- can reference any package module without the env missing dependencies.
                                                                    allPackageSources : List String
                                                                    allPackageSources =
                                                                        topoSortModules moduleGraph pkgModuleNames

                                                                    pkgEnvResult : Result Types.Error Eval.Module.ProjectEnv
                                                                    pkgEnvResult =
                                                                        Eval.Module.buildProjectEnv allPackageSources
                                                                in
                                                                case pkgEnvResult of
                                                                    Err _ ->
                                                                        BackendTask.fail (FatalError.fromString "Failed to build package environment")

                                                                    Ok pkgEnv ->
                                                                        let
                                                                            -- Build baseUserEnv: pkgEnv + all user modules in topo order.
                                                                            -- This is reused for incremental env building in evalWithFileOverrides.
                                                                            userModuleNamesSet : Set String
                                                                            userModuleNamesSet =
                                                                                userParsedFiles |> Dict.keys |> Set.fromList

                                                                            userModulesInOrder : List File
                                                                            userModulesInOrder =
                                                                                topoSortModules moduleGraph userModuleNamesSet
                                                                                    |> List.filterMap (\src -> DepGraph.parseModuleName src)
                                                                                    |> List.filterMap (\name -> Dict.get name userParsedFiles)

                                                                            baseUserEnvResult : Maybe Eval.Module.ProjectEnv
                                                                            baseUserEnvResult =
                                                                                Eval.Module.extendWithFiles pkgEnv userModulesInOrder
                                                                                    |> Result.toMaybe
                                                                        in
                                                                        Cache.inputs allUserPaths
                                                                            |> BackendTask.map
                                                                                (\sourceInputs ->
                                                                                    InterpreterProject
                                                                                        { sourceDirectories = sourceDirectories
                                                                                        , inputsByPath =
                                                                                            sourceInputs
                                                                                                |> List.map
                                                                                                    (\( pathVal, monad ) ->
                                                                                                        ( Path.toString pathVal
                                                                                                        , ( pathVal, monad )
                                                                                                        )
                                                                                                    )
                                                                                                |> Dict.fromList
                                                                                        , userFileContents =
                                                                                            userFileContents
                                                                                                |> Dict.fromList
                                                                                        , depGraph = depGraph
                                                                                        , patchedPackageSources = patchedPackageSources
                                                                                        , extraSources =
                                                                                            extraFileContents
                                                                                                |> List.map Tuple.second
                                                                                        , moduleGraph = moduleGraph
                                                                                        , packageModuleNames = pkgModuleNames
                                                                                        , packageEnv = pkgEnv
                                                                                        , baseUserEnv = baseUserEnvResult
                                                                                        , semanticIndex =
                                                                                            userFileContents
                                                                                                |> List.map
                                                                                                    (\( filePath, content ) ->
                                                                                                        { moduleName =
                                                                                                            DepGraph.parseModuleName content
                                                                                                                |> Maybe.withDefault filePath
                                                                                                        , source = content
                                                                                                        }
                                                                                                    )
                                                                                                |> SemanticHash.buildMultiModuleIndex
                                                                                        }
                                                                                )
                                                            )
                                                )
                                    )
                        )
            )


{-| Read the "source-directories" field from elm.json in the given project directory.
-}
readSourceDirectories : Path -> BackendTask FatalError (List String)
readSourceDirectories projectDir =
    File.rawFile (Path.toString projectDir ++ "/elm.json")
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\raw ->
                case Decode.decodeString (Decode.field "source-directories" (Decode.list Decode.string)) raw of
                    Ok dirs ->
                        BackendTask.succeed dirs

                    Err err ->
                        BackendTask.fail (FatalError.fromString ("Failed to decode source-directories from elm.json: " ++ Decode.errorToString err))
            )


{-| Evaluate a `"ModuleName.valueName"` expression and cache the result.

Parses the expression to determine the module name, then delegates to
`evalWith` with that single import.

-}
eval : InterpreterProject -> String -> (FileOrDirectory -> Cache.Monad a) -> Cache.Monad a
eval project expression k =
    case parseExpression expression of
        Just ( moduleName, _ ) ->
            evalWith project { imports = [ moduleName ], expression = expression } k

        Nothing ->
            Cache.fail ("Invalid expression: " ++ expression ++ " (expected \"ModuleName.valueName\")")


{-| Evaluate an arbitrary Elm expression with multiple imports via the interpreter.

The expression can reference any of the imported modules and must produce
a `String`. Transitive dependencies are computed as the union across all
imported modules. The result is cached based on the combined hash of
package sources, extra sources, relevant user sources, and the wrapper module.

Package modules are parsed once during `loadWith` and reused across all
`evalWith` calls. Only user-specific sources are parsed per call.

-}
evalWith : InterpreterProject -> { imports : List String, expression : String } -> (FileOrDirectory -> Cache.Monad a) -> Cache.Monad a
evalWith (InterpreterProject project) { imports, expression } k =
    let
        -- Get transitive user-source deps as the union across all imported modules
        transDeps : Set String
        transDeps =
            imports
                |> List.filterMap (DepGraph.moduleNameToFilePath project.depGraph)
                |> List.map (DepGraph.transitiveDeps project.depGraph)
                |> List.foldl Set.union Set.empty

        -- Filter inputsByPath to relevant source deps
        relevantInputs : List ( String, Cache.Monad FileOrDirectory )
        relevantInputs =
            project.inputsByPath
                |> Dict.toList
                |> List.filter
                    (\( path, _ ) ->
                        Set.member path transDeps
                    )
                |> List.map (\( path, ( _, monad ) ) -> ( path, monad ))

        -- Generate wrapper module
        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        -- Compute transitive module deps from the wrapper's imports through the module graph
        wrapperImports : Set String
        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules : Set String
        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        -- Topologically sort needed modules and collect their sources
        filteredSources : List String
        filteredSources =
            topoSortModules project.moduleGraph neededModules

        -- Filter to only non-package (user) sources
        userFilteredSources : List String
        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )

        -- All sources for hashing (includes everything for correct cache keys)
        allFilteredSources : List String
        allFilteredSources =
            filteredSources ++ [ wrapperSource ]

        -- Single blob of all filtered sources for hashing
        filteredBlob : String
        filteredBlob =
            String.join "\n---MODULE_SEPARATOR---\n" allFilteredSources
    in
    -- Hash the filtered blob
    Cache.do (Cache.writeFile filteredBlob Cache.succeed) <| \filteredHash ->
    -- Resolve all relevant user file hashes
    Cache.do
        (relevantInputs
            |> List.map
                (\( path, monad ) ->
                    Cache.do monad <| \hash ->
                    Cache.succeed { filename = Path.path path, hash = hash }
                )
            |> Cache.sequence
        )
    <| \sourceFiles ->
    -- Combine all hashes into a single combined hash
    Cache.do
        (Cache.combine
            (sourceFiles
                ++ [ { filename = Path.path "filtered.blob", hash = filteredHash }
                   ]
            )
        )
    <| \combinedHash ->
    -- Cache the interpreter computation
    Cache.compute [ "interpret" ]
        combinedHash
        (\() ->
            let
                -- Only parse user sources + wrapper; reuse the pre-built package env
                result : Result Types.Error Types.Value
                result =
                    Eval.Module.evalWithEnv
                        project.packageEnv
                        (userFilteredSources ++ [ wrapperSource ])
                        (FunctionOrValue [] "results")
            in
            case result of
                Ok (Types.String s) ->
                    s

                Ok other ->
                    "ERROR: Expected String result"

                Err (Types.ParsingError _) ->
                    "ERROR: Parsing error"

                Err (Types.EvalError evalErr) ->
                    "ERROR: Eval error: "
                        ++ evalErrorKindToString evalErr.error
                        ++ " [module: "
                        ++ String.join "." evalErr.currentModule
                        ++ "] [stack: "
                        ++ (evalErr.callStack |> List.take 10 |> List.map (\ref -> String.join "." ref.moduleName ++ "." ++ ref.name) |> String.join " <- ")
                        ++ "]"
        )
        k



{-| Evaluate an expression with source overrides.

Like `evalWith`, but allows overriding specific module sources before
evaluation. Override sources take precedence over existing modules with
the same name. Useful for mutation testing, where you want to evaluate
the same expression with a modified version of one module.

The cache key includes the override source hashes, so each unique
mutation gets its own cached result.

-}
evalWithSourceOverrides : InterpreterProject -> { imports : List String, expression : String, sourceOverrides : List String } -> (FileOrDirectory -> Cache.Monad a) -> Cache.Monad a
evalWithSourceOverrides (InterpreterProject project) { imports, expression, sourceOverrides } k =
    let
        transDeps : Set String
        transDeps =
            imports
                |> List.filterMap (DepGraph.moduleNameToFilePath project.depGraph)
                |> List.map (DepGraph.transitiveDeps project.depGraph)
                |> List.foldl Set.union Set.empty

        relevantInputs : List ( String, Cache.Monad FileOrDirectory )
        relevantInputs =
            project.inputsByPath
                |> Dict.toList
                |> List.filter (\( path, _ ) -> Set.member path transDeps)
                |> List.map (\( path, ( _, monad ) ) -> ( path, monad ))

        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        wrapperImports : Set String
        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules : Set String
        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        filteredSources : List String
        filteredSources =
            topoSortModules project.moduleGraph neededModules

        userFilteredSources : List String
        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )

        -- Include overrides in the content blob for hashing
        allFilteredSources : List String
        allFilteredSources =
            filteredSources ++ sourceOverrides ++ [ wrapperSource ]

        filteredBlob : String
        filteredBlob =
            String.join "\n---MODULE_SEPARATOR---\n" allFilteredSources
    in
    Cache.do (Cache.writeFile filteredBlob Cache.succeed) <| \filteredHash ->
    Cache.do
        (relevantInputs
            |> List.map
                (\( path, monad ) ->
                    Cache.do monad <| \hash ->
                    Cache.succeed { filename = Path.path path, hash = hash }
                )
            |> Cache.sequence
        )
    <| \sourceFiles ->
    Cache.do
        (Cache.combine
            (sourceFiles
                ++ [ { filename = Path.path "filtered.blob", hash = filteredHash } ]
            )
        )
    <| \combinedHash ->
    Cache.compute [ "interpret-with-overrides" ]
        combinedHash
        (\() ->
            let
                -- User sources first, then overrides (which replace same-named
                -- modules since evalWithEnv processes in order, last wins).
                -- Wrapper goes last for import resolution.
                result : Result Types.Error Types.Value
                result =
                    Eval.Module.evalWithEnv
                        project.packageEnv
                        (userFilteredSources ++ sourceOverrides ++ [ wrapperSource ])
                        (FunctionOrValue [] "results")
            in
            case result of
                Ok (Types.String s) ->
                    s

                Ok _ ->
                    "ERROR: Expected String result"

                Err (Types.ParsingError _) ->
                    "ERROR: Parsing error"

                Err (Types.EvalError evalErr) ->
                    "ERROR: Eval error: "
                        ++ evalErrorKindToString evalErr.error
                        ++ " [module: "
                        ++ String.join "." evalErr.currentModule
                        ++ "] [stack: "
                        ++ (evalErr.callStack |> List.take 10 |> List.map (\ref -> String.join "." ref.moduleName ++ "." ++ ref.name) |> String.join " <- ")
                        ++ "]"
        )
        k



{-| Evaluate an expression with pre-parsed File AST overrides.

Like `evalWithSourceOverrides`, but accepts `File` ASTs directly instead of
source strings for the overrides. Skips the write→parse round-trip for mutations.

The `sourceOverrides` are the string sources that still need parsing (e.g. SimpleTestRunner).
The `fileOverrides` are pre-parsed ASTs (e.g. mutated File from Mutator).

-}
evalWithFileOverrides :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , fileOverrides : List { file : File, hashKey : String }
        }
    -> (FileOrDirectory -> Cache.Monad a)
    -> Cache.Monad a
evalWithFileOverrides (InterpreterProject project) { imports, expression, sourceOverrides, fileOverrides } k =
    let
        transDeps : Set String
        transDeps =
            imports
                |> List.filterMap (DepGraph.moduleNameToFilePath project.depGraph)
                |> List.map (DepGraph.transitiveDeps project.depGraph)
                |> List.foldl Set.union Set.empty

        relevantInputs : List ( String, Cache.Monad FileOrDirectory )
        relevantInputs =
            project.inputsByPath
                |> Dict.toList
                |> List.filter (\( path, _ ) -> Set.member path transDeps)
                |> List.map (\( path, ( _, monad ) ) -> ( path, monad ))

        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        wrapperImports : Set String
        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules : Set String
        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        filteredSources : List String
        filteredSources =
            topoSortModules project.moduleGraph neededModules

        userFilteredSources : List String
        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )

        -- Pre-parsed user module Files (from loadWith), in topo order
        userFilteredFiles : List File
        userFilteredFiles =
            neededModules
                |> Set.toList
                |> List.filter (\name -> not (Set.member name project.packageModuleNames))
                |> List.filterMap (\name -> Dict.get name project.moduleGraph.moduleToFile)

        -- Semantic hash: hash the wrapper expression's semantic dependencies
        -- + override hash keys. Only invalidates when actually-called functions change.
        fileOverrideHashKeys : List String
        fileOverrideHashKeys =
            fileOverrides |> List.map .hashKey

        semanticCacheKey : String
        semanticCacheKey =
            let
                -- Extract function references from the wrapper expression to find
                -- entry-point declarations, then use their semantic hashes (which
                -- transitively include all dependencies via the Merkle property)
                wrapperDeps =
                    case Elm.Parser.parseToFile wrapperSource of
                        Ok wrapperFile ->
                            wrapperFile.declarations
                                |> List.concatMap
                                    (\(Node _ decl) ->
                                        case decl of
                                            FunctionDeclaration func ->
                                                SemanticHash.extractDependencies
                                                    (Node
                                                        { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } }
                                                        (Node.value func.declaration |> .expression |> Node.value)
                                                    )

                                            _ ->
                                                []
                                    )

                        Err _ ->
                            []

                -- Resolve each dependency to its semantic hash from the index
                entryPointHashes =
                    wrapperDeps
                        |> List.filterMap
                            (\( modName, funcName ) ->
                                let
                                    qualName =
                                        if List.isEmpty modName then
                                            funcName

                                        else
                                            String.join "." modName ++ "." ++ funcName
                                in
                                SemanticHash.semanticHashForEntry project.semanticIndex qualName
                            )
                        |> List.sort

                overrideKey =
                    String.join "|" fileOverrideHashKeys

                sourceOverrideKey =
                    String.join "|" (List.map (\s -> String.left 100 s) sourceOverrides)
            in
            String.join "\n"
                (entryPointHashes ++ [ overrideKey, sourceOverrideKey, wrapperSource ])
    in
    Cache.do (Cache.writeFile semanticCacheKey Cache.succeed) <| \semanticHash ->
    Cache.compute [ "interpret-with-file-overrides" ]
        semanticHash
        (\() ->
            let
                -- Only parse sourceOverrides and wrapper (small/new);
                -- user sources are pre-parsed in moduleGraph.moduleToFile
                parsedOverrides : Result Types.Error (List File)
                parsedOverrides =
                    (sourceOverrides ++ [ wrapperSource ])
                        |> List.map
                            (\src ->
                                Elm.Parser.parseToFile src
                                    |> Result.mapError Types.ParsingError
                            )
                        |> combineFileResults
            in
            case parsedOverrides of
                Err err ->
                    case err of
                        Types.ParsingError _ ->
                            "ERROR: Parsing error"

                        Types.EvalError evalErr ->
                            "ERROR: Eval error: " ++ evalErrorKindToString evalErr.error

                Ok overrideFiles ->
                    let
                        overridesButWrapper =
                            List.take (List.length overrideFiles - 1) overrideFiles

                        wrapperFile =
                            List.drop (List.length overrideFiles - 1) overrideFiles

                        -- Incremental env: if baseUserEnv is available, replace only
                        -- the mutated module(s) instead of rebuilding from all user files.
                        result : Result Types.Error Types.Value
                        result =
                            case project.baseUserEnv of
                                Just baseEnv ->
                                    let
                                        -- Replace each file override in the base env
                                        replacedEnvResult =
                                            fileOverrides
                                                |> List.foldl
                                                    (\override envRes ->
                                                        envRes
                                                            |> Result.andThen
                                                                (\env ->
                                                                    Eval.Module.replaceModuleInEnv env
                                                                        { file = override.file
                                                                        , moduleName = Eval.Module.fileModuleName override.file
                                                                        , interface = Eval.Module.buildInterfaceFromFile override.file
                                                                        }
                                                                )
                                                    )
                                                    (Ok baseEnv)

                                        -- Only need sourceOverrides + wrapper as additional files
                                        additionalFiles =
                                            overridesButWrapper ++ wrapperFile
                                    in
                                    case replacedEnvResult of
                                        Ok updatedEnv ->
                                            Eval.Module.evalWithEnvFromFilesAndLimit
                                                (Just 5000000)
                                                updatedEnv
                                                additionalFiles
                                                (FunctionOrValue [] "results")

                                        Err e ->
                                            Err e

                                Nothing ->
                                    -- Fallback: original path (all user files)
                                    let
                                        allFiles =
                                            userFilteredFiles
                                                ++ overridesButWrapper
                                                ++ List.map .file fileOverrides
                                                ++ wrapperFile
                                    in
                                    Eval.Module.evalWithEnvFromFilesAndLimit
                                        (Just 5000000)
                                        project.packageEnv
                                        allFiles
                                        (FunctionOrValue [] "results")
                    in
                    case result of
                        Ok (Types.String s) ->
                            s

                        Ok _ ->
                            "ERROR: Expected String result"

                        Err (Types.ParsingError _) ->
                            "ERROR: Parsing error"

                        Err (Types.EvalError evalErr) ->
                            "ERROR: Eval error: "
                                ++ evalErrorKindToString evalErr.error
        )
        k


{-| Evaluate a test expression with tracing to collect coverage data.

Runs the test suite once (unmutated) with the interpreter's trace mode enabled,
then walks the resulting CallTree to extract all evaluated source ranges.
Returns both the test result string and the list of covered ranges.

-}
evalWithCoverage :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        }
    -> BackendTask FatalError { result : String, coveredRanges : List Range }
evalWithCoverage (InterpreterProject project) { imports, expression, sourceOverrides } =
    let
        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        wrapperImports : Set String
        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules : Set String
        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        filteredSources : List String
        filteredSources =
            topoSortModules project.moduleGraph neededModules

        userFilteredSources : List String
        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )
    in
    let
        allSources =
            userFilteredSources ++ sourceOverrides ++ [ wrapperSource ]

        ( result, callTrees, _ ) =
            Eval.Module.traceWithEnv
                project.packageEnv
                allSources
                (FunctionOrValue [] "results")

        resultString =
            case result of
                Ok (Types.String s) ->
                    s

                Ok _ ->
                    "ERROR: Expected String result"

                Err (Types.ParsingError _) ->
                    "ERROR: Parsing error"

                Err (Types.EvalError evalErr) ->
                    "ERROR: Eval error: " ++ evalErrorKindToString evalErr.error

        coveredRanges =
            Coverage.extractRanges callTrees
    in
    BackendTask.succeed { result = resultString, coveredRanges = coveredRanges }


combineFileResults : List (Result e a) -> Result e (List a)
combineFileResults results =
    List.foldr
        (\result acc ->
            Result.map2 (::) result acc
        )
        (Ok [])
        results


evalErrorKindToString : Types.EvalErrorKind -> String
evalErrorKindToString kind =
    case kind of
        Types.TypeError msg ->
            "type error: " ++ msg

        Types.Unsupported msg ->
            "unsupported: " ++ msg

        Types.NameError name ->
            "could not resolve '" ++ name ++ "'"

        Types.Todo msg ->
            "hit Debug.todo: " ++ msg

        Types.TailCall _ ->
            "internal TCO signal (should not be user-visible)"


{-| Get the pre-built package environment. Useful for direct eval calls
that bypass the caching layer (e.g. benchmarking).
-}
getPackageEnv : InterpreterProject -> Eval.Module.ProjectEnv
getPackageEnv (InterpreterProject project) =
    project.packageEnv


{-| Get the dependency graph for the project's user source files.
-}
getDepGraph : InterpreterProject -> DepGraph.Graph
getDepGraph (InterpreterProject project) =
    project.depGraph


{-| Prepare the source lists needed for eval, without actually evaluating.

Returns `allSources` (everything needed, for `evalProject`) and
`userSources` (only non-package modules, for `evalWithEnv`).

-}
prepareEvalSources :
    InterpreterProject
    -> { imports : List String, expression : String }
    -> { allSources : List String, userSources : List String }
prepareEvalSources (InterpreterProject project) { imports, expression } =
    let
        wrapperSource =
            generateWrapper imports expression

        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        filteredSources =
            topoSortModules project.moduleGraph neededModules

        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )
    in
    { allSources = filteredSources ++ [ wrapperSource ]
    , userSources = userFilteredSources ++ [ wrapperSource ]
    }


{-| Prepare and evaluate an expression, pure function without Cache.Monad.

Uses baseUserEnv when available (skips re-parsing all user modules).
Only parses sourceOverrides + the generated wrapper module.

Useful when the caller manages caching externally (e.g. using semantic hash
keys instead of content-based hashing).

-}
prepareAndEval :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String }
    -> Result String String
prepareAndEval (InterpreterProject project) { imports, expression, sourceOverrides } =
    let
        wrapperSource =
            generateWrapper imports expression

        -- Parse only sourceOverrides + wrapper (small, new each time).
        -- User modules are already in baseUserEnv.
        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults

        result =
            case parsedNewModules of
                Err err ->
                    Err err

                Ok newFiles ->
                    case project.baseUserEnv of
                        Just baseEnv ->
                            -- Fast path: baseUserEnv already has all user modules.
                            -- Only need to add sourceOverrides + wrapper.
                            Eval.Module.evalWithEnvFromFiles baseEnv newFiles (FunctionOrValue [] "results")

                        Nothing ->
                            -- Fallback: parse everything from scratch
                            let
                                { userSources } =
                                    prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                                allSources =
                                    let
                                        len =
                                            List.length userSources

                                        beforeWrapper =
                                            List.take (len - 1) userSources

                                        wrapper =
                                            List.drop (len - 1) userSources
                                    in
                                    beforeWrapper ++ sourceOverrides ++ wrapper
                            in
                            Eval.Module.evalWithEnv project.packageEnv allSources (FunctionOrValue [] "results")
    in
    formatEvalResult result


{-| Like prepareAndEval but returns the raw interpreter Value.

Useful when the expression returns structured data (e.g. a Tuple)
that the caller needs to decompose.

-}
prepareAndEvalRaw :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String }
    -> Result String Types.Value
prepareAndEvalRaw (InterpreterProject project) { imports, expression, sourceOverrides } =
    let
        wrapperSource =
            generateWrapper imports expression

        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults
    in
    case parsedNewModules of
        Err err ->
            Err (formatError err)

        Ok newFiles ->
            case project.baseUserEnv of
                Just baseEnv ->
                    Eval.Module.evalWithEnvFromFiles baseEnv newFiles (FunctionOrValue [] "results")
                        |> Result.mapError formatError

                Nothing ->
                    let
                        { userSources } =
                            prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                        allSources =
                            let
                                len =
                                    List.length userSources

                                beforeWrapper =
                                    List.take (len - 1) userSources

                                wrapper =
                                    List.drop (len - 1) userSources
                            in
                            beforeWrapper ++ sourceOverrides ++ wrapper
                    in
                    Eval.Module.evalWithEnv project.packageEnv allSources (FunctionOrValue [] "results")
                        |> Result.mapError formatError


{-| Like prepareAndEval but with injected Values available in the expression.

The injected values are available as local variables. Used to pass
preserved interpreter state (like elm-review's updatedRules) into
subsequent evaluations.

-}
prepareAndEvalWithValues :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, injectedValues : FastDict.Dict String Types.Value }
    -> Result String Types.Value
prepareAndEvalWithValues (InterpreterProject project) { imports, expression, sourceOverrides, injectedValues } =
    let
        wrapperSource =
            generateWrapper imports expression

        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults
    in
    case parsedNewModules of
        Err err ->
            Err (formatError err)

        Ok newFiles ->
            case project.baseUserEnv of
                Just baseEnv ->
                    Eval.Module.evalWithEnvFromFilesAndValues baseEnv newFiles injectedValues (FunctionOrValue [] "results")
                        |> Result.mapError formatError

                Nothing ->
                    -- Fallback without values (can't inject without baseUserEnv)
                    Eval.Module.evalWithEnvFromFiles project.packageEnv newFiles (FunctionOrValue [] "results")
                        |> Result.mapError formatError


{-| Evaluate with intercepts that can yield to the framework.

When an intercept yields (EvYield tag payload resume), the yieldHandler
BackendTask runs with (tag, payload), producing a Value. The eval then
resumes with that Value via the continuation.

This is the BackendTask driver loop: eval → yield → handle → resume → repeat.
-}
prepareRawEvalWithYield :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, intercepts : FastDict.Dict String Types.Intercept, injectedValues : FastDict.Dict String Types.Value }
    -> Types.EvalResult Types.Value
prepareRawEvalWithYield (InterpreterProject project) { imports, expression, sourceOverrides, intercepts, injectedValues } =
    let
        wrapperSource =
            generateWrapper imports expression

        allSources =
            let
                { userSources } =
                    prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                len =
                    List.length userSources

                beforeWrapper =
                    List.take (len - 1) userSources

                wrapper =
                    List.drop (len - 1) userSources
            in
            beforeWrapper ++ sourceOverrides ++ wrapper

        evalSources =
            case project.baseUserEnv of
                Just _ ->
                    sourceOverrides ++ [ wrapperSource ]

                Nothing ->
                    allSources

        env =
            case project.baseUserEnv of
                Just baseEnv ->
                    baseEnv

                Nothing ->
                    project.packageEnv

        rawResult =
            if FastDict.isEmpty injectedValues then
                Eval.Module.evalWithInterceptsRaw env evalSources intercepts (FunctionOrValue [] "results")

            else
                -- Need to inject values AND use intercepts.
                -- Parse sources, build env, inject values, then eval with intercepts.
                -- This is a combination of evalWithEnvFromFilesAndValues + evalWithInterceptsRaw.
                let
                    parseResult =
                        evalSources
                            |> List.map
                                (\src ->
                                    Elm.Parser.parseToFile src
                                        |> Result.mapError Types.ParsingError
                                )
                            |> combineFileResults
                in
                case parseResult of
                    Err _ ->
                        Types.EvErr { currentModule = [], callStack = [], error = Types.TypeError "Parse error in prepareAndEvalWithYield" }

                    Ok parsedModules ->
                        Eval.Module.evalWithEnvFromFilesAndValuesAndInterceptsRaw env parsedModules injectedValues intercepts (FunctionOrValue [] "results")
    in
    rawResult


{-| Evaluate with intercepts that can yield to the framework.

When an intercept yields (EvYield tag payload resume), the yieldHandler
BackendTask runs with (tag, payload), producing a Value. The eval then
resumes with that Value via the continuation.

This is the BackendTask driver loop: eval → yield → handle → resume → repeat.
-}
prepareAndEvalWithYield :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, intercepts : FastDict.Dict String Types.Intercept, injectedValues : FastDict.Dict String Types.Value }
    -> (String -> Types.Value -> BackendTask FatalError Types.Value)
    -> BackendTask FatalError (Result String Types.Value)
prepareAndEvalWithYield project evalConfig yieldHandler =
    driveYields yieldHandler (prepareRawEvalWithYield project evalConfig)


{-| Like `prepareAndEvalWithYield`, but threads caller-managed state through the
yield loop. Useful for in-memory caching experiments where the state should
live outside the interpreter and be updated on each yield.
-}
prepareAndEvalWithYieldState :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, intercepts : FastDict.Dict String Types.Intercept, injectedValues : FastDict.Dict String Types.Value }
    -> state
    -> (state -> String -> Types.Value -> BackendTask FatalError ( state, Types.Value ))
    -> BackendTask FatalError ( Result String Types.Value, state )
prepareAndEvalWithYieldState project evalConfig initialState yieldHandler =
    driveYieldsState initialState yieldHandler (prepareRawEvalWithYield project evalConfig)


{-| Drive the yield loop: handle EvYield via BackendTask, resume, repeat.
-}
driveYields :
    (String -> Types.Value -> BackendTask FatalError Types.Value)
    -> Types.EvalResult Types.Value
    -> BackendTask FatalError (Result String Types.Value)
driveYields yieldHandler evalResult =
    case evalResult of
        Types.EvOk value ->
            BackendTask.succeed (Ok value)

        Types.EvErr evalErr ->
            BackendTask.succeed (Err (formatError (Types.EvalError evalErr)))

        Types.EvOkTrace value _ _ ->
            BackendTask.succeed (Ok value)

        Types.EvErrTrace evalErr _ _ ->
            BackendTask.succeed (Err (formatError (Types.EvalError evalErr)))

        Types.EvYield tag payload resume ->
            -- Handle the yield via BackendTask, then resume eval
            yieldHandler tag payload
                |> BackendTask.andThen
                    (\resumeValue ->
                        driveYields yieldHandler (resume resumeValue)
                    )


driveYieldsState :
    state
    -> (state -> String -> Types.Value -> BackendTask FatalError ( state, Types.Value ))
    -> Types.EvalResult Types.Value
    -> BackendTask FatalError ( Result String Types.Value, state )
driveYieldsState state yieldHandler evalResult =
    case evalResult of
        Types.EvOk value ->
            BackendTask.succeed ( Ok value, state )

        Types.EvErr evalErr ->
            BackendTask.succeed ( Err (formatError (Types.EvalError evalErr)), state )

        Types.EvOkTrace value _ _ ->
            BackendTask.succeed ( Ok value, state )

        Types.EvErrTrace evalErr _ _ ->
            BackendTask.succeed ( Err (formatError (Types.EvalError evalErr)), state )

        Types.EvYield tag payload resume ->
            yieldHandler state tag payload
                |> BackendTask.andThen
                    (\( nextState, resumeValue ) ->
                        driveYieldsState nextState yieldHandler (resume resumeValue)
                    )


{-| Like prepareAndEval but with function intercepts (synchronous, no yield support).

Intercepts are checked before normal function evaluation. Used for
elm-review cache markers, memoization, and framework callbacks.
-}
prepareAndEvalWithIntercepts :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, intercepts : FastDict.Dict String Types.Intercept }
    -> Result String Types.Value
prepareAndEvalWithIntercepts (InterpreterProject project) { imports, expression, sourceOverrides, intercepts } =
    let
        wrapperSource =
            generateWrapper imports expression

        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults
    in
    case parsedNewModules of
        Err err ->
            Err (formatError err)

        Ok newFiles ->
            let
                -- Build sources list for evalWithIntercepts
                { userSources } =
                    prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                allSources =
                    let
                        len =
                            List.length userSources

                        beforeWrapper =
                            List.take (len - 1) userSources

                        wrapper =
                            List.drop (len - 1) userSources
                    in
                    beforeWrapper ++ sourceOverrides ++ wrapper
            in
            case project.baseUserEnv of
                Just baseEnv ->
                    -- Fast path: baseUserEnv has all user modules pre-loaded.
                    -- Only parse sourceOverrides + wrapper (small/new).
                    Eval.Module.evalWithIntercepts baseEnv (sourceOverrides ++ [ wrapperSource ]) intercepts (FunctionOrValue [] "results")
                        |> Result.mapError formatError

                Nothing ->
                    -- Fallback: parse everything from scratch
                    Eval.Module.evalWithIntercepts project.packageEnv allSources intercepts (FunctionOrValue [] "results")
                        |> Result.mapError formatError


formatEvalResult : Result Types.Error Types.Value -> Result String String
formatEvalResult result =
    case result of
        Ok (Types.String s) ->
            Ok s

        Ok _ ->
            Err "ERROR: Expected String result"

        Err err ->
            Err (formatError err)


formatError : Types.Error -> String
formatError err =
    case err of
        Types.ParsingError _ ->
            "ERROR: Parsing error"

        Types.EvalError evalErr ->
            "ERROR: Eval error: "
                ++ evalErrorKindToString evalErr.error
                ++ " [module: "
                ++ String.join "." evalErr.currentModule
                ++ "] [stack: "
                ++ (evalErr.callStack |> List.take 10 |> List.map (\ref -> String.join "." ref.moduleName ++ "." ++ ref.name) |> String.join " <- ")
                ++ "]"



-- HELPERS


{-| Parse "ModuleName.valueName" into its parts.
-}
parseExpression : String -> Maybe ( String, String )
parseExpression expr =
    let
        trimmed =
            String.trim expr
    in
    case String.split "." trimmed |> List.reverse of
        valueName :: modulePartsReversed ->
            if List.isEmpty modulePartsReversed then
                Nothing

            else
                let
                    moduleName =
                        modulePartsReversed |> List.reverse |> String.join "."
                in
                if String.isEmpty valueName || String.isEmpty moduleName then
                    Nothing

                else
                    Just ( moduleName, valueName )

        _ ->
            Nothing


{-| Generate a wrapper module that imports the given modules and evaluates
the expression as `results : String`.
-}
generateWrapper : List String -> String -> String
generateWrapper imports expression =
    let
        importLines : String
        importLines =
            imports
                |> List.map (\m -> "import " ++ m)
                |> String.join "\n"
    in
    "module InterpreterWrapper__ exposing (results)\n\n"
        ++ importLines
        ++ "\n\n\nresults : String\nresults =\n    "
        ++ expression
        ++ "\n"


{-| BFS through the module import graph to find all transitively needed modules.
-}
transitiveModuleDeps : Dict String (Set String) -> Set String -> Set String
transitiveModuleDeps importsGraph rootSet =
    bfsModules importsGraph (Set.toList rootSet) rootSet


bfsModules : Dict String (Set String) -> List String -> Set String -> Set String
bfsModules importsGraph queue visited =
    case queue of
        [] ->
            visited

        current :: rest ->
            let
                directDeps =
                    Dict.get current importsGraph
                        |> Maybe.withDefault Set.empty

                newDeps =
                    Set.diff directDeps visited

                newQueue =
                    rest ++ Set.toList newDeps

                newVisited =
                    Set.union visited newDeps
            in
            bfsModules importsGraph newQueue newVisited


{-| Topologically sort the needed modules using DFS post-order,
returning their sources in dependency order.
-}
topoSortModules : ModuleGraph -> Set String -> List String
topoSortModules graph needed =
    let
        dfs :
            String
            -> { visited : Set String, order : List String }
            -> { visited : Set String, order : List String }
        dfs moduleName acc =
            if Set.member moduleName acc.visited then
                acc

            else
                let
                    withVisited =
                        { acc | visited = Set.insert moduleName acc.visited }

                    deps =
                        Dict.get moduleName graph.imports
                            |> Maybe.withDefault Set.empty
                            |> Set.toList
                            |> List.filter (\dep -> Set.member dep needed)

                    afterDeps =
                        List.foldl dfs withVisited deps
                in
                case Dict.get moduleName graph.moduleToSource of
                    Just src ->
                        { afterDeps | order = afterDeps.order ++ [ src ] }

                    Nothing ->
                        afterDeps

        result =
            Set.toList needed
                |> List.foldl dfs { visited = Set.empty, order = [] }
    in
    result.order
