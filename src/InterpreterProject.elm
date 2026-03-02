module InterpreterProject exposing (InterpreterProject, load, loadWith, eval, evalWith)

{-| Evaluate and cache Elm expressions via the pure Elm interpreter.

Mirrors `ElmProject` structurally but replaces `elm make` + `node` with
`Cache.compute` + `Eval.Module.evalProject`.

-}

import BackendTask exposing (BackendTask)
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Glob as Glob
import Cache exposing (FileOrDirectory)
import DepGraph
import Dict exposing (Dict)
import Elm.Syntax.Expression exposing (Expression(..))
import Eval.Module
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Path exposing (Path)
import ProjectSources
import Set exposing (Set)
import Types


type alias ModuleGraph =
    { moduleToSource : Dict String String
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
                ProjectSources.loadPackageDeps
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

                                                                    moduleGraph : ModuleGraph
                                                                    moduleGraph =
                                                                        { moduleToSource = Dict.fromList allModules
                                                                        , imports =
                                                                            allModules
                                                                                |> List.map
                                                                                    (\( name, src ) ->
                                                                                        ( name, DepGraph.parseImports src |> Set.fromList )
                                                                                    )
                                                                                |> Dict.fromList
                                                                        }
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

        -- All sources for hashing and evaluation: filtered modules + wrapper
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
                result : Result Types.Error Types.Value
                result =
                    Eval.Module.evalProject
                        allFilteredSources
                        (FunctionOrValue [] "results")
            in
            case result of
                Ok (Types.String s) ->
                    s

                Ok other ->
                    "ERROR: Expected String result, got: " ++ Debug.toString other

                Err (Types.ParsingError deadEnds) ->
                    "ERROR: Parsing error: " ++ Debug.toString deadEnds

                Err (Types.EvalError evalErr) ->
                    "ERROR: Eval error: " ++ Debug.toString evalErr.error
        )
        k



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
