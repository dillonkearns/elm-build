module ElmProject exposing (ElmProject, fromPath, eval, evalWith)

{-| Evaluate and cache Elm expressions.

Given an Elm expression like `"SampleValue.greeting"`, this module:

1.  Derives the cache key from the expression and its transitive source dependencies
2.  Generates a wrapper module that outputs the value via a port
3.  Compiles with `elm make` and runs with `node`
4.  Caches the result so subsequent evaluations are instant

-}

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BackendTask.Glob as Glob
import Cache exposing (FileOrDirectory)
import DepGraph
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import Path exposing (Path)
import Set exposing (Set)


type ElmProject
    = ElmProject
        { projectDir : Path
        , sourceDirectories : List String
        , inputsByPath : Dict String ( Path, Cache.Monad FileOrDirectory )
        , depGraph : DepGraph.Graph
        , elmJsonContent : String
        }


{-| Initialize an ElmProject from a project directory.

Globs for all `.elm` files under `src/`, reads their contents for
dependency analysis, builds a dependency graph, and hashes all inputs.

-}
fromPath : Path -> BackendTask FatalError ElmProject
fromPath projectDir =
    let
        p : String
        p =
            Path.toString projectDir

        sourceDirectories : List String
        sourceDirectories =
            [ "src", "codegen" ]
    in
    Glob.fromStringWithOptions
        (let
            o : Glob.Options
            o =
                Glob.defaultOptions
         in
         { o | include = Glob.OnlyFiles }
        )
        (p ++ "/src/**/*.elm")
        |> BackendTask.andThen
            (\files ->
                let
                    elmFiles : List String
                    elmFiles =
                        files
                            |> List.filter (\f -> not (String.contains "elm-stuff" f))
                            |> List.sort
                in
                let
                    elmJsonPath : String
                    elmJsonPath =
                        p ++ "/elm.json"
                in
                File.rawFile elmJsonPath
                    |> BackendTask.allowFatal
                    |> BackendTask.andThen
                        (\elmJsonRaw ->
                            elmFiles
                                |> List.map
                                    (\filePath ->
                                        File.rawFile filePath
                                            |> BackendTask.allowFatal
                                            |> BackendTask.map (\content -> ( filePath, content ))
                                    )
                                |> BackendTask.sequence
                                |> BackendTask.andThen
                                    (\fileContents ->
                                        let
                                            depGraph : DepGraph.Graph
                                            depGraph =
                                                DepGraph.buildGraph
                                                    { sourceDirectories = sourceDirectories
                                                    , files =
                                                        fileContents
                                                            |> List.map
                                                                (\( filePath, content ) ->
                                                                    { filePath = stripDotSlash filePath
                                                                    , content = content
                                                                    }
                                                                )
                                                    }

                                            allPaths : List Path
                                            allPaths =
                                                (elmFiles ++ [ elmJsonPath ])
                                                    |> List.sort
                                                    |> List.map Path.path
                                        in
                                        Cache.inputs allPaths
                                            |> BackendTask.map
                                                (\sourceInputs ->
                                                    ElmProject
                                                        { projectDir = projectDir
                                                        , sourceDirectories = sourceDirectories
                                                        , inputsByPath =
                                                            sourceInputs
                                                                |> List.map
                                                                    (\( pathVal, monad ) ->
                                                                        ( stripDotSlash (Path.toString pathVal)
                                                                        , ( pathVal, monad )
                                                                        )
                                                                    )
                                                                |> Dict.fromList
                                                        , depGraph = depGraph
                                                        , elmJsonContent = elmJsonRaw
                                                        }
                                                )
                                    )
                        )
            )


{-| Evaluate an Elm expression and cache the result.

The expression must be of the form `"ModuleName.valueName"` where
the value has type `String`.

The result is cached based on the transitive source dependencies of
the module, so changing an unrelated file won't cause re-evaluation.

-}
eval : ElmProject -> String -> (FileOrDirectory -> Cache.Monad a) -> Cache.Monad a
eval project expression k =
    case parseExpression expression of
        Just ( moduleName, _ ) ->
            evalWith project { imports = [ moduleName ], expression = expression } k

        Nothing ->
            Cache.fail ("Invalid expression: " ++ expression ++ " (expected \"ModuleName.valueName\")")


{-| Evaluate an arbitrary Elm expression with multiple imports.

The expression can reference any of the imported modules and must produce
a `String`. Transitive dependencies are computed as the union across all
imported modules. Package-only modules (not in the dep graph) are silently
skipped for dependency tracking — `elm make` resolves them.

-}
evalWith : ElmProject -> { imports : List String, expression : String } -> (FileOrDirectory -> Cache.Monad a) -> Cache.Monad a
evalWith (ElmProject project) { imports, expression } k =
    let
        -- Get transitive deps as the union across all imported modules
        transDeps : Set String
        transDeps =
            imports
                |> List.filterMap (DepGraph.moduleNameToFilePath project.depGraph)
                |> List.map (DepGraph.transitiveDeps project.depGraph)
                |> List.foldl Set.union Set.empty

        -- Filter inputsByPath to relevant source deps (excluding elm.json —
        -- we generate a workspace-clean elm.json separately)
        relevantInputs : List ( String, Cache.Monad FileOrDirectory )
        relevantInputs =
            project.inputsByPath
                |> Dict.toList
                |> List.filter
                    (\( normalizedPath, _ ) ->
                        Set.member normalizedPath transDeps
                    )
                |> List.map (\( normalizedPath, ( _, monad ) ) -> ( normalizedPath, monad ))

        -- Generate a workspace-clean elm.json with only ["src"] as source dirs.
        -- The eval workspace only has src/ so we strip any extra source-directories
        -- (like elm-interpreter paths) that don't exist in the workspace.
        workspaceElmJson : String
        workspaceElmJson =
            cleanSourceDirectories project.elmJsonContent

        -- Generate the wrapper module source
        wrapperSource : String
        wrapperSource =
            generateWrapperWith imports expression

        -- Shell script: ensure source dirs exist, compile + run
        mkdirs : String
        mkdirs =
            project.sourceDirectories
                |> List.map (\d -> "mkdir -p " ++ d)
                |> String.join " && "

        script : String
        script =
            mkdirs
                ++ " && elm make --output=elm.js src/EvalWrapper__.elm 1>&2 && node -e \"const {Elm}=require('./elm.js');Elm.EvalWrapper__.init().ports.evalOutput__.subscribe(v=>{process.stdout.write(v);process.exit(0)})\""
    in
    Cache.do (Cache.writeFile wrapperSource Cache.succeed) <| \wrapperHash ->
    Cache.do (Cache.writeFile workspaceElmJson Cache.succeed) <| \elmJsonHash ->
    -- Resolve all relevant inputs and combine with wrapper into a project directory
    Cache.do
        (relevantInputs
            |> List.map
                (\( normalizedPath, monad ) ->
                    Cache.do monad <| \hash ->
                    Cache.succeed { filename = Path.path normalizedPath, hash = hash }
                )
            |> Cache.sequence
            |> Cache.andThen
                (\sourceFiles ->
                    Cache.combine
                        (sourceFiles
                            ++ [ { filename = Path.path "src/EvalWrapper__.elm", hash = wrapperHash }
                               , { filename = Path.path "elm.json", hash = elmJsonHash }
                               ]
                        )
                )
        )
    <| \projectDirHash ->
    Cache.commandInWritableDirectory "sh" [ "-c", script ] projectDirHash k


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


{-| Generate a port module that evaluates and outputs an arbitrary expression.

Supports multiple imports so the expression can reference any combination of
modules (e.g., `TestRunnerHelper.runToJson SampleTests.suite`).

-}
generateWrapperWith : List String -> String -> String
generateWrapperWith imports expression =
    String.join "\n"
        ([ "port module EvalWrapper__ exposing (main)"
         , ""
         ]
            ++ List.map (\m -> "import " ++ m) imports
            ++ [ ""
               , "port evalOutput__ : String -> Cmd msg"
               , ""
               , "main ="
               , "    Platform.worker"
               , "        { init = \\() -> ( (), evalOutput__ (" ++ expression ++ ") )"
               , "        , update = \\_ m -> ( m, Cmd.none )"
               , "        , subscriptions = \\_ -> Sub.none"
               , "        }"
               , ""
               ]
        )


{-| Replace the "source-directories" field in elm.json with just ["src"].

The eval workspace only contains a flat `src/` directory, so any extra
source-directories from the host project (like submodule paths) would
cause `elm make` to fail in the workspace.

-}
cleanSourceDirectories : String -> String
cleanSourceDirectories elmJson =
    case String.indexes "\"source-directories\"" elmJson of
        startIdx :: _ ->
            case String.indexes "[" (String.dropLeft startIdx elmJson) of
                bracketStart :: _ ->
                    let
                        afterKey : String
                        afterKey =
                            String.dropLeft (startIdx + bracketStart) elmJson
                    in
                    case String.indexes "]" afterKey of
                        bracketEnd :: _ ->
                            String.left startIdx elmJson
                                ++ "\"source-directories\": [\"src\"]"
                                ++ String.dropLeft (bracketEnd + 1) afterKey

                        _ ->
                            elmJson

                _ ->
                    elmJson

        _ ->
            elmJson


stripDotSlash : String -> String
stripDotSlash s =
    if String.startsWith "./" s then
        String.dropLeft 2 s

    else
        s
