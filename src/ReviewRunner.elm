module ReviewRunner exposing (ReviewError, buildExpression, buildModuleRecord, computeSemanticKey, escapeElmString, parseReviewOutput, run)

{-| Run elm-review rules via the interpreter.

Usage:
    npx elm-pages run src/ReviewRunner.elm

-}

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Glob as Glob
import BackendTask.Time
import Cache
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Dict
import FatalError exposing (FatalError)
import FNV1a
import SemanticHash
import InterpreterProject exposing (InterpreterProject)
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import Set
import Time
import Types


type alias ReviewError =
    { ruleName : String
    , filePath : String
    , line : Int
    , column : Int
    , message : String
    }


type alias Config =
    { reviewDir : String
    , sourceDirs : List String
    , buildDirectory : Path
    }


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.optionalKeywordArg "review-dir"
                        |> Option.map (Maybe.withDefault "review")
                        |> Option.withDescription "Review config directory (default: review)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "source-dirs"
                        |> Option.map (Maybe.map (String.split ",") >> Maybe.withDefault [ "src" ])
                        |> Option.withDescription "Source directories to review (comma-separated, default: src)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "build"
                        |> Option.map (Maybe.withDefault ".elm-review-build" >> Path.path)
                        |> Option.withDescription "Build/cache directory (default: .elm-review-build)"
                    )
            )


run : Script
run =
    Script.withCliOptions programConfig (task >> BackendTask.quiet)



-- PURE HELPERS


{-| Escape a string for embedding as an Elm string literal.
Handles backslash, double quote, newline, and carriage return.
-}
escapeElmString : String -> String
escapeElmString str =
    str
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""
        |> String.replace "\n" "\\n"
        |> String.replace "\u{000D}" "\\r"


{-| Build an Elm record literal for a module: { path = "...", source = "..." }
-}
buildModuleRecord : { path : String, source : String } -> String
buildModuleRecord { path, source } =
    "{ path = \"" ++ escapeElmString path ++ "\", source = \"" ++ escapeElmString source ++ "\" }"


{-| Build the full evaluation expression.
-}
buildExpression : List { path : String, source : String } -> String
buildExpression modules =
    let
        moduleList =
            case modules of
                [] ->
                    "[]"

                _ ->
                    "[ "
                        ++ (modules
                                |> List.map buildModuleRecord
                                |> String.join ", "
                           )
                        ++ " ]"
    in
    "ReviewRunnerHelper.runReview " ++ moduleList


{-| Parse the pipe-delimited error output from ReviewRunnerHelper.
Format: RULE:name|FILE:path|LINE:n|COL:n|MSG:message
The MSG field is last and may contain pipe characters.
-}
parseReviewOutput : String -> List ReviewError
parseReviewOutput output =
    if String.isEmpty (String.trim output) then
        []

    else
        output
            |> String.lines
            |> List.filterMap parseSingleError


parseSingleError : String -> Maybe ReviewError
parseSingleError line =
    -- Split on | but MSG is last and may contain pipes
    -- Format: RULE:x|FILE:x|LINE:x|COL:x|MSG:rest...
    case String.split "|" line of
        ruleField :: fileField :: lineField :: colField :: msgParts ->
            let
                extractField prefix field =
                    if String.startsWith prefix field then
                        Just (String.dropLeft (String.length prefix) field)

                    else
                        Nothing

                -- MSG may contain | chars, so rejoin the remaining parts
                msgField =
                    String.join "|" msgParts
            in
            case
                ( extractField "RULE:" ruleField
                , extractField "FILE:" fileField
                , ( extractField "LINE:" lineField |> Maybe.andThen String.toInt
                  , extractField "COL:" colField |> Maybe.andThen String.toInt
                  , extractField "MSG:" msgField
                  )
                )
            of
                ( Just ruleName, Just filePath, ( Just lineNum, Just colNum, Just message ) ) ->
                    Just
                        { ruleName = ruleName
                        , filePath = filePath
                        , line = lineNum
                        , column = colNum
                        , message = message
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Compute a semantic cache key for a set of source files.

Uses SemanticHash to compute Merkle-style hashes for each declaration
in each file. Whitespace and comment changes don't affect the hash —
only meaningful AST changes do.

This runs on the elm-build side (native compilation, fast) — NOT
through the interpreter.

-}
computeSemanticKey : List { path : String, source : String } -> String
computeSemanticKey files =
    let
        perFileHashes : List String
        perFileHashes =
            files
                |> List.sortBy .path
                |> List.map
                    (\{ path, source } ->
                        let
                            index =
                                SemanticHash.buildIndexFromSource source

                            -- Combine all declaration hashes for this file
                            declHashes =
                                index
                                    |> Dict.toList
                                    |> List.sortBy Tuple.first
                                    |> List.map
                                        (\( name, info ) ->
                                            name ++ ":" ++ info.semanticHash
                                        )
                                    |> String.join ","
                        in
                        path ++ "|" ++ declHashes
                    )
    in
    perFileHashes
        |> String.join "\n"
        |> FNV1a.hash
        |> String.fromInt


{-| The inline ReviewRunnerHelper module source, injected as a source override.
-}
reviewRunnerHelperSource : String
reviewRunnerHelperSource =
    String.join "\n"
        [ "module ReviewRunnerHelper exposing (runReview)"
        , "import Review.Project as Project"
        , "import Review.Rule as Rule"
        , "import NoDebug.Log"
        , "import NoDebug.TodoOrToString"
        , "import NoExposingEverything"
        , "import NoImportingEverything"
        , "import NoMissingTypeAnnotation"
        , "import NoMissingTypeAnnotationInLetIn"
        , "import NoDeprecated"
        , "import Simplify"
        , "import NoUnused.Variables"
        , "import NoUnused.CustomTypeConstructors"
        , "import NoUnused.CustomTypeConstructorArgs"
        , "import NoUnused.Parameters"
        , "import NoUnused.Patterns"
        , ""
        , "runReview modules ="
        , "    let"
        , "        project ="
        , "            List.foldl (\\mod proj -> Project.addModule { path = mod.path, source = mod.source } proj) Project.new modules"
        , "        rules ="
        , "            [ NoDebug.Log.rule"
        , "            , NoDebug.TodoOrToString.rule"
        , "            , NoExposingEverything.rule"
        , "            , NoImportingEverything.rule []"
        , "            , NoMissingTypeAnnotation.rule"
        , "            , NoMissingTypeAnnotationInLetIn.rule"
        , "            , NoDeprecated.rule NoDeprecated.defaults"
        , "            ]"
        , "        ( errors, _ ) ="
        , "            Rule.review rules project"
        , "    in"
        , "    errors |> List.map formatError |> String.join \"\\n\""
        , ""
        , "formatError err ="
        , "    let"
        , "        range = Rule.errorRange err"
        , "    in"
        , "    String.join \"|\""
        , "        [ \"RULE:\" ++ Rule.errorRuleName err"
        , "        , \"FILE:\" ++ Rule.errorFilePath err"
        , "        , \"LINE:\" ++ String.fromInt range.start.row"
        , "        , \"COL:\" ++ String.fromInt range.start.column"
        , "        , \"MSG:\" ++ Rule.errorMessage err"
        , "        ]"
        ]



-- SCRIPT IMPLEMENTATION


task : Config -> BackendTask FatalError ()
task config =
    Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Running elm-review via interpreter") <| \_ ->
    Do.do ensureReviewDeps <| \_ ->
    Do.do
        (BackendTask.Extra.timed "Loading review project" "Loaded review project"
            (InterpreterProject.loadWith
                { projectDir = Path.path "review"
                , skipPackages = Set.union kernelPackages conflictingPackages
                , patchSource = patchSource
                , extraSourceFiles = []
                , sourceDirectories = Just [ "review/src" ]
                }
            )
        )
    <| \reviewProject ->
    Do.do (resolveTargetFiles config) <| \targetFiles ->
    Do.log ("Found " ++ String.fromInt (List.length targetFiles) ++ " source file(s) to review") <| \_ ->
    Do.do (readTargetFiles targetFiles) <| \targetFileContents ->
    let
        semanticKey =
            computeSemanticKey targetFileContents

        expression =
            buildExpression targetFileContents
    in
    Do.log ("Semantic key: " ++ semanticKey) <| \_ ->
    Do.do BackendTask.Time.now <| \startTime ->
    Do.do
        (Cache.run { jobs = Nothing } config.buildDirectory
            (InterpreterProject.evalWithSourceOverrides reviewProject
                { imports = [ "ReviewRunnerHelper" ]
                , expression = expression
                , sourceOverrides = [ reviewRunnerHelperSource ]
                }
                Cache.succeed
            )
        )
    <| \cacheResult ->
    Do.allowFatal (File.rawFile (Path.toString cacheResult.output)) <| \output ->
    Do.do BackendTask.Time.now <| \endTime ->
    let
        evalMs =
            Time.posixToMillis endTime - Time.posixToMillis startTime

        errors =
            if String.startsWith "ERROR:" output then
                []

            else
                parseReviewOutput output
    in
    if String.startsWith "ERROR:" output then
        Do.log (Ansi.Color.fontColor Ansi.Color.red ("Interpreter error: " ++ String.left 200 output)) <| \_ ->
        BackendTask.fail (FatalError.fromString output)

    else if List.isEmpty errors then
        Do.log
            (Ansi.Color.fontColor Ansi.Color.brightGreen
                ("\nNo errors found! (" ++ String.fromInt evalMs ++ "ms)")
            )
        <| \_ ->
        Do.noop

    else
        Do.do (displayErrors errors) <| \_ ->
        Do.log
            (Ansi.Color.fontColor Ansi.Color.brightRed
                ("\n" ++ String.fromInt (List.length errors) ++ " error(s) found (" ++ String.fromInt evalMs ++ "ms)")
            )
        <| \_ ->
        BackendTask.fail
            (FatalError.build
                { title = "Review Errors"
                , body = String.fromInt (List.length errors) ++ " errors found"
                }
            )


displayErrors : List ReviewError -> BackendTask FatalError ()
displayErrors errors =
    errors
        |> List.map
            (\err ->
                Script.log
                    (Ansi.Color.fontColor Ansi.Color.red
                        ("  " ++ err.ruleName ++ " - " ++ err.filePath ++ ":" ++ String.fromInt err.line ++ ":" ++ String.fromInt err.column)
                        ++ "\n    "
                        ++ err.message
                    )
            )
        |> BackendTask.Extra.sequence_


resolveTargetFiles : Config -> BackendTask FatalError (List String)
resolveTargetFiles config =
    config.sourceDirs
        |> List.map (\dir -> dir ++ "/**/*.elm")
        |> List.map
            (\globPattern ->
                Glob.fromStringWithOptions
                    (let
                        o =
                            Glob.defaultOptions
                     in
                     { o | include = Glob.OnlyFiles }
                    )
                    globPattern
            )
        |> BackendTask.Extra.combine
        |> BackendTask.map (List.concat >> List.sort)


readTargetFiles : List String -> BackendTask FatalError (List { path : String, source : String })
readTargetFiles files =
    files
        |> List.map
            (\filePath ->
                File.rawFile filePath
                    |> BackendTask.allowFatal
                    |> BackendTask.map (\content -> { path = filePath, source = content })
            )
        |> BackendTask.Extra.combine


ensureReviewDeps : BackendTask FatalError ()
ensureReviewDeps =
    Script.exec "elm" [ "make", "src/ReviewConfig.elm", "--output", "/dev/null" ]
        |> BackendTask.inDir "review"
        |> BackendTask.toResult
        |> BackendTask.map (\_ -> ())


kernelPackages : Set.Set String
kernelPackages =
    Set.fromList
        [ "elm/html"
        , "elm/virtual-dom"
        , "elm/browser"
        , "elm/http"
        , "elm/file"
        , "elm/url"
        ]


{-| Packages to skip because they cause module name collisions
(multiple packages expose `Util`, `Char.Extra`, etc.) in the
interpreter's flat namespace. Only skip packages not needed
for the rules we actually use.
-}
conflictingPackages : Set.Set String
conflictingPackages =
    Set.fromList
        [ -- Util collisions
          "truqu/elm-review-nobooleancase"
        , "SiriusStarr/elm-review-no-single-pattern-case"
        , "SiriusStarr/elm-review-no-unsorted"
        , "lue-bird/elm-review-equals-caseable"
        , "lue-bird/elm-review-no-catch-all-for-specific-remaining-patterns"
        , "lue-bird/elm-review-variables-between-case-of-access-in-cases"
        , "lue-bird/elm-no-record-type-alias-constructor-function"

        -- Char.Extra / other collisions
        , "miniBill/elm-rope"
        , "gampleman/elm-review-derive"
        , "dillonkearns/elm-review-html-to-elm"
        , "matzko/elm-review-limit-aliased-record-size"
        , "sparksp/elm-review-camelcase"
        , "sparksp/elm-review-imports"
        , "sparksp/elm-review-ports"
        , "miniBill/elm-review-no-broken-elm-parser-functions"
        , "miniBill/elm-review-no-internal-imports"
        , "miniBill/elm-review-validate-regexes"
        , "folq/review-rgb-ranges"
        , "SiriusStarr/elm-review-pipeline-styles"
        , "vkfisher/elm-review-no-unsafe-division"
        , "lue-bird/elm-review-documentation-code-snippet"
        ]


patchSource : String -> String
patchSource source =
    if String.contains "runThunk =\n    Elm.Kernel.Test.runThunk" source then
        source
            |> String.replace
                "runThunk =\n    Elm.Kernel.Test.runThunk"
                "runThunk fn =\n    Ok (fn ())"

    else
        source
