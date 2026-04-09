module ProjectSources exposing (loadPackageDeps, loadPackageDepsCached, loadProjectSources, resolvePackageVersions)

{-| Load all source files for a project and its dependencies.

Reads the project elm.json, resolves all dependencies from ELM\_HOME,
topologically sorts packages, and returns all source file contents in
dependency order (packages first, then user sources).

-}

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.Env
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Glob as Glob
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode
import Pages.Script as Script
import Path exposing (Path)
import Result.Extra
import Set exposing (Set)


readFilesTask : List String -> BackendTask FatalError (List String)
readFilesTask paths =
    BackendTask.Custom.run "readFiles"
        (Json.Encode.list Json.Encode.string paths)
        (Decode.list Decode.string)
        |> BackendTask.allowFatal


ensureDirTask : String -> BackendTask FatalError ()
ensureDirTask dirPath =
    BackendTask.Custom.run "ensureDir"
        (Json.Encode.string dirPath)
        (Decode.succeed ())
        |> BackendTask.allowFatal


{-| Load all project sources in dependency order.

Returns a list of source file contents where:

  - Package dependency sources come first (topologically sorted)
  - User project sources come last
  - elm/core is skipped (it's built into the interpreter)

`userSourceDirectories` specifies which directories contain user code
to interpret (e.g. `["src"]`). Package dependencies are resolved from elm.json.

`targetFile` is the path to the module that should be evaluated. It will be
placed last in the returned list so `evalProject` uses it as the eval context.

-}
loadProjectSources : { projectDir : Path, userSourceDirectories : List String, targetFile : String, skipPackages : Set String } -> BackendTask FatalError (List String)
loadProjectSources { projectDir, userSourceDirectories, targetFile, skipPackages } =
    let
        projectPath : String
        projectPath =
            Path.toString projectDir
    in
    Do.allowFatal (File.rawFile (projectPath ++ "/elm.json")) <| \elmJsonRaw ->
    Do.do (decodeElmJson elmJsonRaw) <| \elmJson ->
    Do.do resolveElmHome <| \elmHome ->
    Do.do
        (resolvePackageVersions elmHome
            (Dict.union elmJson.directDeps elmJson.indirectDeps
                |> Dict.remove "elm/core"
                |> Dict.filter (\name _ -> not (Set.member name skipPackages))
            )
        )
    <| \directDeps ->
    -- Recursively discover transitive deps (packages depend on other packages)
    Do.do (resolveTransitiveDeps elmHome directDeps) <| \allDepsWithKernel ->
    let
        allDeps =
            allDepsWithKernel
                |> Dict.filter (\name _ -> not (Set.member name skipPackages))
    in
    Do.do (loadPackageDepGraphs elmHome allDeps) <| \pkgDepGraphs ->
    let
        sortedPackageNames : List String
        sortedPackageNames =
            topoSort allDeps pkgDepGraphs
    in
    Do.do (loadPackageSources elmHome allDeps sortedPackageNames) <| \packageSources ->
    let
        -- Merge source directories from elm.json with any extra dirs from the caller.
        -- Deduplicate so we don't load the same dir twice.
        allUserSourceDirs =
            (elmJson.sourceDirectories ++ userSourceDirectories)
                |> Set.fromList
                |> Set.toList
    in
    Do.do (loadUserSources projectPath allUserSourceDirs) <| \userSources ->
    let
        targetPath =
            if String.startsWith "/" targetFile then
                targetFile

            else
                projectPath ++ "/" ++ targetFile
    in
    Do.allowFatal (File.rawFile targetPath) <| \targetSource ->
    let
        -- Remove the target file from userSources (it was already read by glob)
        -- and append it at the end so evalProject uses it as the eval context
        userSourcesWithoutTarget : List String
        userSourcesWithoutTarget =
            List.filter (\s -> s /= targetSource) userSources
    in
    BackendTask.succeed (packageSources ++ userSourcesWithoutTarget ++ [ targetSource ])


{-| Load package dependency sources from a project's elm.json, with the ability
to skip specific packages (e.g. those with unsupported kernel code).

Returns source file contents in topologically sorted dependency order.

-}
loadPackageDeps : { projectDir : Path, skipPackages : Set String } -> BackendTask FatalError (List String)
loadPackageDeps { projectDir, skipPackages } =
    let
        projectPath : String
        projectPath =
            Path.toString projectDir
    in
    Do.allowFatal (File.rawFile (projectPath ++ "/elm.json")) <| \elmJsonRaw ->
    Do.do (decodeElmJson elmJsonRaw) <| \elmJson ->
    Do.do resolveElmHome <| \elmHome ->
    Do.do
        (resolvePackageVersions elmHome
            (Dict.union elmJson.directDeps elmJson.indirectDeps
                |> Dict.remove "elm/core"
            )
        )
    <| \directDeps ->
    Do.do (resolveTransitiveDeps elmHome directDeps) <| \allDepsWithKernel ->
    let
        allDeps =
            allDepsWithKernel
                |> Dict.filter (\name _ -> not (Set.member name skipPackages))
    in
    Do.do (loadPackageDepGraphs elmHome allDeps) <| \pkgDepGraphs ->
    let
        sortedPackageNames : List String
        sortedPackageNames =
            topoSort allDeps pkgDepGraphs
    in
    Do.do (loadPackageSources elmHome allDeps sortedPackageNames) <| \packageSources ->
    BackendTask.succeed packageSources


{-| Like `loadPackageDeps` but caches raw package sources on disk.

Cache key is elm.json content + sorted skipPackages. On hit, reads the
cached blob (~5ms) instead of re-globbing ELM\_HOME (~150ms).

-}
loadPackageDepsCached :
    { projectDir : Path, skipPackages : Set String }
    -> BackendTask FatalError (List String)
loadPackageDepsCached { projectDir, skipPackages } =
    let
        projectPath : String
        projectPath =
            Path.toString projectDir

        cacheDir : String
        cacheDir =
            projectPath ++ "/.elm-build"

        cacheKeyPath : String
        cacheKeyPath =
            cacheDir ++ "/package-sources.key"

        cacheBlobPath : String
        cacheBlobPath =
            cacheDir ++ "/package-sources.blob"

        separator : String
        separator =
            "\n---PKG_SEPARATOR---\n"
    in
    Do.allowFatal (File.rawFile (projectPath ++ "/elm.json")) <| \elmJsonContent ->
    let
        expectedKey : String
        expectedKey =
            elmJsonContent ++ "\n---SKIP---\n" ++ (skipPackages |> Set.toList |> String.join ",")
    in
    File.rawFile cacheKeyPath
        |> BackendTask.toResult
        |> BackendTask.andThen
            (\keyResult ->
                case keyResult of
                    Ok storedKey ->
                        if storedKey == expectedKey then
                            File.rawFile cacheBlobPath
                                |> BackendTask.allowFatal
                                |> BackendTask.map
                                    (\blob ->
                                        if String.isEmpty blob then
                                            []

                                        else
                                            String.split separator blob
                                    )

                        else
                            loadAndWriteCache expectedKey separator cacheDir cacheKeyPath cacheBlobPath projectDir skipPackages

                    Err _ ->
                        loadAndWriteCache expectedKey separator cacheDir cacheKeyPath cacheBlobPath projectDir skipPackages
            )


loadAndWriteCache : String -> String -> String -> String -> String -> Path -> Set String -> BackendTask FatalError (List String)
loadAndWriteCache expectedKey separator cacheDir cacheKeyPath cacheBlobPath projectDir skipPackages =
    Do.do (loadPackageDeps { projectDir = projectDir, skipPackages = skipPackages }) <| \sources ->
    Do.do (ensureDirTask cacheDir) <| \_ ->
    Do.allowFatal (Script.writeFile { path = cacheKeyPath, body = expectedKey }) <| \_ ->
    Do.allowFatal (Script.writeFile { path = cacheBlobPath, body = String.join separator sources }) <| \_ ->
    BackendTask.succeed sources


{-| Decoded fields from the project elm.json.
-}
type alias ElmJsonDeps =
    { sourceDirectories : List String
    , directDeps : Dict String String
    , indirectDeps : Dict String String
    }


decodeElmJson : String -> BackendTask FatalError ElmJsonDeps
decodeElmJson raw =
    case Decode.decodeString elmJsonDecoder raw of
        Ok deps ->
            BackendTask.succeed deps

        Err err ->
            BackendTask.fail (FatalError.fromString ("Failed to decode elm.json: " ++ Decode.errorToString err))


elmJsonDecoder : Decode.Decoder ElmJsonDeps
elmJsonDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\elmJsonType ->
                if elmJsonType == "package" then
                    packageElmJsonDecoder

                else
                    applicationElmJsonDecoder
            )


applicationElmJsonDecoder : Decode.Decoder ElmJsonDeps
applicationElmJsonDecoder =
    Decode.map5
        (\sourceDirs directDeps indirectDeps testDirectDeps testIndirectDeps ->
            ElmJsonDeps
                sourceDirs
                (Dict.union directDeps testDirectDeps)
                (Dict.union indirectDeps testIndirectDeps)
        )
        (Decode.field "source-directories" (Decode.list Decode.string))
        (Decode.at [ "dependencies", "direct" ] (Decode.dict Decode.string))
        (Decode.at [ "dependencies", "indirect" ] (Decode.dict Decode.string))
        (Decode.oneOf
            [ Decode.at [ "test-dependencies", "direct" ] (Decode.dict Decode.string)
            , Decode.succeed Dict.empty
            ]
        )
        (Decode.oneOf
            [ Decode.at [ "test-dependencies", "indirect" ] (Decode.dict Decode.string)
            , Decode.succeed Dict.empty
            ]
        )


{-| Package elm.json has version ranges, not exact versions.
We store the ranges and resolve them later against ELM\_HOME.
-}
packageElmJsonDecoder : Decode.Decoder ElmJsonDeps
packageElmJsonDecoder =
    Decode.map2
        (\deps testDeps ->
            ElmJsonDeps
                [ "src" ]
                deps
                testDeps
        )
        (Decode.field "dependencies" (Decode.dict Decode.string))
        (Decode.oneOf
            [ Decode.field "test-dependencies" (Decode.dict Decode.string)
            , Decode.succeed Dict.empty
            ]
        )


{-| Resolve version ranges to installed versions.

Application elm.json has exact versions ("1.1.4") — these pass through unchanged.
Package elm.json has ranges ("1.0.0 <= v < 2.0.0") — these are resolved to the
latest installed version in ELM\_HOME.

-}
resolvePackageVersions : String -> Dict String String -> BackendTask FatalError (Dict String String)
resolvePackageVersions elmHome deps =
    deps
        |> Dict.toList
        |> List.map
            (\( pkgName, versionOrRange ) ->
                if String.isEmpty versionOrRange || String.contains "<" versionOrRange || String.contains ">" versionOrRange then
                    -- It's a version range — resolve to latest installed
                    let
                        pkgDir =
                            elmHome ++ "/0.19.1/packages/" ++ pkgName
                    in
                    Glob.fromStringWithOptions
                        (let
                            o =
                                Glob.defaultOptions
                         in
                         { o | include = Glob.OnlyFolders }
                        )
                        (pkgDir ++ "/*")
                        |> BackendTask.andThen
                            (\versions ->
                                case versions |> List.sort |> List.reverse |> List.head of
                                    Just latestPath ->
                                        let
                                            version =
                                                latestPath
                                                    |> String.split "/"
                                                    |> List.reverse
                                                    |> List.head
                                                    |> Maybe.withDefault versionOrRange
                                        in
                                        BackendTask.succeed ( pkgName, version )

                                    Nothing ->
                                        BackendTask.fail
                                            (FatalError.fromString
                                                ("Package " ++ pkgName ++ " not installed. Run elm install first.")
                                            )
                            )

                else
                    -- Exact version — pass through
                    BackendTask.succeed ( pkgName, versionOrRange )
            )
        |> BackendTask.Extra.combine
        |> BackendTask.map Dict.fromList


{-| Recursively discover transitive package dependencies.

Reads each package's elm.json, discovers its deps, resolves their versions,
and repeats until no new packages are found. This ensures all transitive
dependencies are included even for package-type projects.

-}
resolveTransitiveDeps : String -> Dict String String -> BackendTask FatalError (Dict String String)
resolveTransitiveDeps elmHome knownDeps =
    let
        knownDepList =
            Dict.toList knownDeps

        elmJsonPaths =
            knownDepList
                |> List.map
                    (\( pkgName, version ) ->
                        packageBasePath elmHome pkgName version ++ "/elm.json"
                    )
    in
    readFilesTask elmJsonPaths
        |> BackendTask.map
            (\rawElmJsons ->
                rawElmJsons
                    |> List.map
                        (\raw ->
                            case Decode.decodeString packageDepsDecoder raw of
                                Ok depNames ->
                                    depNames

                                Err _ ->
                                    Set.empty
                        )
            )
        |> BackendTask.map (List.foldl Set.union Set.empty)
        |> BackendTask.andThen
            (\transDepNames ->
                let
                    newDeps =
                        transDepNames
                            |> Set.toList
                            |> List.filter
                                (\name ->
                                    not (Dict.member name knownDeps)
                                        && name
                                        /= "elm/core"
                                )
                in
                if List.isEmpty newDeps then
                    BackendTask.succeed knownDeps

                else
                    -- Resolve versions for newly discovered deps, then recurse
                    resolvePackageVersions elmHome
                        (newDeps |> List.map (\n -> ( n, "" )) |> Dict.fromList)
                        |> BackendTask.andThen
                            (\resolvedNew ->
                                resolveTransitiveDeps elmHome (Dict.union knownDeps resolvedNew)
                            )
            )


{-| Resolve ELM\_HOME — check the environment variable, fall back to $HOME/.elm.
-}
resolveElmHome : BackendTask FatalError String
resolveElmHome =
    BackendTask.map2
        (\maybeElmHome maybeHome ->
            case maybeElmHome of
                Just elmHome ->
                    elmHome

                Nothing ->
                    case maybeHome of
                        Just home ->
                            home ++ "/.elm"

                        Nothing ->
                            "~/.elm"
        )
        (BackendTask.Env.get "ELM_HOME")
        (BackendTask.Env.get "HOME")


{-| For each package, read its elm.json to discover its dependency names.
Returns a Dict from package name to its set of dependency names.
-}
loadPackageDepGraphs : String -> Dict String String -> BackendTask FatalError (Dict String (Set String))
loadPackageDepGraphs elmHome allDeps =
    let
        depList =
            Dict.toList allDeps

        elmJsonPaths =
            depList
                |> List.map
                    (\( pkgName, version ) ->
                        packageBasePath elmHome pkgName version ++ "/elm.json"
                    )
    in
    readFilesTask elmJsonPaths
        |> BackendTask.andThen
            (\rawElmJsons ->
                List.map3
                    (\( pkgName, _ ) pkgElmJsonPath raw ->
                        case Decode.decodeString packageDepsDecoder raw of
                            Ok deps ->
                                Ok ( pkgName, deps )

                            Err err ->
                                Err
                                    (FatalError.fromString
                                        ("Failed to decode " ++ pkgElmJsonPath ++ ": " ++ Decode.errorToString err)
                                    )
                    )
                    depList
                    elmJsonPaths
                    rawElmJsons
                    |> Result.Extra.combine
                    |> BackendTask.fromResult
            )
        |> BackendTask.map Dict.fromList


packageDepsDecoder : Decode.Decoder (Set String)
packageDepsDecoder =
    Decode.field "dependencies" (Decode.dict Decode.string)
        |> Decode.map (\d -> Dict.keys d |> Set.fromList)


{-| Build path to a package's src directory.
Package name "elm/json" with version "1.1.4" becomes
`{elmHome}/0.19.1/packages/elm/json/1.1.4/src`
-}
packageBasePath : String -> String -> String -> String
packageBasePath elmHome pkgName version =
    elmHome ++ "/0.19.1/packages/" ++ pkgName ++ "/" ++ version


packageSrcPath : String -> String -> String -> String
packageSrcPath elmHome pkgName version =
    packageBasePath elmHome pkgName version ++ "/src"


{-| Topological sort of packages by their dependencies.
Packages whose deps aren't in our set (e.g. elm/core) are treated as satisfied.
-}
topoSort : Dict String String -> Dict String (Set String) -> List String
topoSort allDeps pkgDepGraphs =
    let
        packageNames : Set String
        packageNames =
            Dict.keys allDeps |> Set.fromList

        visit :
            String
            -> { sorted : List String, visited : Set String }
            -> { sorted : List String, visited : Set String }
        visit name acc =
            if Set.member name acc.visited then
                acc

            else
                let
                    deps : Set String
                    deps =
                        Dict.get name pkgDepGraphs
                            |> Maybe.withDefault Set.empty
                            |> Set.intersect packageNames

                    afterDeps : { sorted : List String, visited : Set String }
                    afterDeps =
                        Set.foldl visit
                            { acc | visited = Set.insert name acc.visited }
                            deps
                in
                { afterDeps | sorted = afterDeps.sorted ++ [ name ] }
    in
    Set.foldl visit { sorted = [], visited = Set.empty } packageNames
        |> .sorted


{-| Load all .elm source files for the given packages in order.
-}
loadPackageSources : String -> Dict String String -> List String -> BackendTask FatalError (List String)
loadPackageSources elmHome allDeps sortedPackageNames =
    sortedPackageNames
        |> List.map
            (\pkgName ->
                case Dict.get pkgName allDeps of
                    Just version ->
                        loadSourcesFromDir (packageSrcPath elmHome pkgName version)

                    Nothing ->
                        BackendTask.succeed []
            )
        |> BackendTask.Extra.sequence
        |> BackendTask.map List.concat


{-| Load all .elm source files from user source directories.
-}
loadUserSources : String -> List String -> BackendTask FatalError (List String)
loadUserSources projectPath sourceDirectories =
    sourceDirectories
        |> List.map (\dir -> loadSourcesFromDir (projectPath ++ "/" ++ dir))
        |> BackendTask.Extra.combine
        |> BackendTask.map List.concat


{-| Glob for all .elm files in a directory and read their contents.
-}
loadSourcesFromDir : String -> BackendTask FatalError (List String)
loadSourcesFromDir dir =
    Glob.fromStringWithOptions
        (let
            o : Glob.Options
            o =
                Glob.defaultOptions
         in
         { o | include = Glob.OnlyFiles }
        )
        (dir ++ "/**/*.elm")
        |> BackendTask.andThen
            (\files ->
                files
                    |> List.sort
                    |> readFilesTask
            )
