module InterpreterProject exposing (InterpreterProject, load, eval, evalWith)

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
import Path exposing (Path)
import Set exposing (Set)
import Types


type InterpreterProject
    = InterpreterProject
        { sourceDirectories : List String
        , inputsByPath : Dict String ( Path, Cache.Monad FileOrDirectory )
        , userFileContents : Dict String String
        , depGraph : DepGraph.Graph
        , patchedPackageSources : List String
        , extraSources : List String
        }


{-| Initialize an InterpreterProject.

Globs for user source files, reads their contents for dependency analysis
and interpreter evaluation, builds a dependency graph, and hashes all
user source files via `Cache.inputs`.

-}
load :
    { projectDir : Path
    , sourceDirectories : List String
    , userSourceGlobs : List String
    , extraSourceFiles : List String
    , patchedPackageSources : List String
    }
    -> BackendTask FatalError InterpreterProject
load config =
    -- Glob all user source globs for .elm files
    config.userSourceGlobs
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
                                                    { sourceDirectories = config.sourceDirectories
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
                                        in
                                        Cache.inputs allUserPaths
                                            |> BackendTask.map
                                                (\sourceInputs ->
                                                    InterpreterProject
                                                        { sourceDirectories = config.sourceDirectories
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
                                                        , patchedPackageSources = config.patchedPackageSources
                                                        , extraSources =
                                                            extraFileContents
                                                                |> List.map Tuple.second
                                                        }
                                                )
                                    )
                        )
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

        -- Relevant user file contents for the interpreter thunk
        relevantUserSources : List String
        relevantUserSources =
            project.inputsByPath
                |> Dict.keys
                |> List.filter (\path -> Set.member path transDeps)
                |> List.filterMap (\path -> Dict.get path project.userFileContents)

        -- Generate wrapper module
        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        -- Package sources as a single blob for hashing
        packageBlob : String
        packageBlob =
            String.join "\n---MODULE_SEPARATOR---\n" project.patchedPackageSources

        -- Extra sources as a single blob for hashing
        extrasBlob : String
        extrasBlob =
            String.join "\n---MODULE_SEPARATOR---\n" project.extraSources
    in
    -- Hash the package blob, extras blob, and wrapper module
    Cache.do (Cache.writeFile packageBlob Cache.succeed) <| \packageHash ->
    Cache.do (Cache.writeFile extrasBlob Cache.succeed) <| \extrasHash ->
    Cache.do (Cache.writeFile wrapperSource Cache.succeed) <| \wrapperHash ->
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
                ++ [ { filename = Path.path "packages.blob", hash = packageHash }
                   , { filename = Path.path "extras.blob", hash = extrasHash }
                   , { filename = Path.path "wrapper.elm", hash = wrapperHash }
                   ]
            )
        )
    <| \combinedHash ->
    -- Cache the interpreter computation
    Cache.compute [ "interpret" ]
        combinedHash
        (\() ->
            let
                allSources : List String
                allSources =
                    project.patchedPackageSources
                        ++ project.extraSources
                        ++ relevantUserSources
                        ++ [ wrapperSource ]

                result : Result Types.Error Types.Value
                result =
                    Eval.Module.evalProject
                        allSources
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
