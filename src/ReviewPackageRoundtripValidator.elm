module ReviewPackageRoundtripValidator exposing (run)

import AstWireCodec
import BackendTask exposing (BackendTask)
import Bytes
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Elm.Syntax.File
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node as Node
import Eval.Module
import FatalError exposing (FatalError)
import Json.Encode
import Lamdera.Wire3
import Pages.Script as Script exposing (Script)
import Path
import ProjectSources
import ReviewRunner
import Set


type alias Config =
    { reviewDir : String
    , limit : Int
    }


type alias ValidationResult =
    { moduleName : String
    , encodedBytes : Int
    , status : ValidationStatus
    }


type ValidationStatus
    = DecodeFailed
    | JsonMismatch MismatchDetails
    | ExactMatch


type alias MismatchDetails =
    { originalJsonBytes : Int
    , decodedJsonBytes : Int
    , firstDiffIndex : Maybe Int
    , moduleKindBefore : String
    , moduleKindAfter : String
    , importsBefore : Int
    , importsAfter : Int
    , declarationsBefore : Int
    , declarationsAfter : Int
    , commentsBefore : Int
    , commentsAfter : Int
    }


type alias ValidationSummary =
    { totalModules : Int
    , exactMatches : Int
    , jsonMismatches : List ValidationResult
    , decodeFailures : List ValidationResult
    , aggregateDecodeOk : Bool
    , firstFailingPrefixLength : Maybe Int
    , firstFailingPrefixModule : Maybe String
    }


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.optionalKeywordArg "review-dir"
                        |> Option.map (Maybe.withDefault "bench/review")
                        |> Option.withDescription "Review project directory to validate (default: bench/review)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "limit"
                        |> Option.map
                            (\maybeLimit ->
                                maybeLimit
                                    |> Maybe.andThen String.toInt
                                    |> Maybe.map (max 1)
                                    |> Maybe.withDefault 12
                            )
                        |> Option.withDescription "Maximum mismatches/failures to print (default: 12)"
                    )
            )


run : Script
run =
    Script.withCliOptions programConfig (task >> BackendTask.quiet)


task : Config -> BackendTask FatalError ()
task config =
    ProjectSources.loadPackageDepsCached
        { projectDir = Path.path config.reviewDir
        , skipPackages = Set.union ReviewRunner.kernelPackages ReviewRunner.conflictingPackages
        }
        |> BackendTask.andThen
            (\packageSources ->
                let
                    patchedSources =
                        List.map ReviewRunner.patchSource packageSources
                in
                case Eval.Module.parseProjectSources patchedSources of
                    Err _ ->
                        BackendTask.fail (FatalError.fromString "Failed to parse review package sources")

                    Ok parsedSources ->
                        parsedSources
                            |> summarizeRoundtrip
                            |> formatSummary config.limit
                            |> Script.log
                            |> BackendTask.allowFatal
            )


summarizeRoundtrip : List { a | file : Elm.Syntax.File.File, moduleName : List String } -> ValidationSummary
summarizeRoundtrip parsedSources =
    let
        results =
            List.map validateParsedSource parsedSources

        decodeFailures =
            List.filter
                (\result ->
                    case result.status of
                        DecodeFailed ->
                            True

                        _ ->
                            False
                )
                results

        jsonMismatches =
            List.filter
                (\result ->
                    case result.status of
                        JsonMismatch _ ->
                            True

                        _ ->
                            False
                )
                results

        exactMatches =
            List.length results - List.length decodeFailures - List.length jsonMismatches

        files =
            List.map .file parsedSources

        aggregateDecodeOk =
            let
                aggregateDecoded =
                    files
                        |> Lamdera.Wire3.encodeList AstWireCodec.encodeFile
                        |> Lamdera.Wire3.bytesEncode
                        |> Lamdera.Wire3.bytesDecodeStrict
                            (Lamdera.Wire3.decodeList AstWireCodec.decodeFile)
            in
            aggregateDecoded /= Nothing

        firstFailingPrefixLength =
            if aggregateDecodeOk then
                Nothing

            else
                findFirstFailingPrefix files 1

        firstFailingPrefixModule =
            firstFailingPrefixLength
                |> Maybe.andThen
                    (\prefixLength ->
                        parsedSources
                            |> List.drop (prefixLength - 1)
                            |> List.head
                            |> Maybe.map (.moduleName >> String.join ".")
                    )
    in
    { totalModules = List.length results
    , exactMatches = exactMatches
    , jsonMismatches = jsonMismatches
    , decodeFailures = decodeFailures
    , aggregateDecodeOk = aggregateDecodeOk
    , firstFailingPrefixLength = firstFailingPrefixLength
    , firstFailingPrefixModule = firstFailingPrefixModule
    }


findFirstFailingPrefix : List Elm.Syntax.File.File -> Int -> Maybe Int
findFirstFailingPrefix files prefixLength =
    if prefixLength > List.length files then
        Nothing

    else
        let
            prefixDecoded =
                files
                    |> List.take prefixLength
                    |> Lamdera.Wire3.encodeList AstWireCodec.encodeFile
                    |> Lamdera.Wire3.bytesEncode
                    |> Lamdera.Wire3.bytesDecodeStrict
                        (Lamdera.Wire3.decodeList AstWireCodec.decodeFile)

            prefixOk =
                prefixDecoded /= Nothing
        in
        if prefixOk then
            findFirstFailingPrefix files (prefixLength + 1)

        else
            Just prefixLength


validateParsedSource : { a | file : Elm.Syntax.File.File, moduleName : List String } -> ValidationResult
validateParsedSource parsedSource =
    let
        encodedBytes =
            AstWireCodec.encodeToBytes parsedSource.file

        moduleName =
            String.join "." parsedSource.moduleName
    in
    case AstWireCodec.decodeFromBytes encodedBytes of
        Nothing ->
            { moduleName = moduleName
            , encodedBytes = bytesWidth encodedBytes
            , status = DecodeFailed
            }

        Just decodedFile ->
            let
                originalJson =
                    Json.Encode.encode 0 (Elm.Syntax.File.encode parsedSource.file)

                decodedJson =
                    Json.Encode.encode 0 (Elm.Syntax.File.encode decodedFile)
            in
            if originalJson == decodedJson then
                { moduleName = moduleName
                , encodedBytes = bytesWidth encodedBytes
                , status = ExactMatch
                }

            else
                { moduleName = moduleName
                , encodedBytes = bytesWidth encodedBytes
                , status =
                    JsonMismatch
                        { originalJsonBytes = String.length originalJson
                        , decodedJsonBytes = String.length decodedJson
                        , firstDiffIndex = firstDiff originalJson decodedJson 0
                        , moduleKindBefore = moduleKind parsedSource.file
                        , moduleKindAfter = moduleKind decodedFile
                        , importsBefore = List.length parsedSource.file.imports
                        , importsAfter = List.length decodedFile.imports
                        , declarationsBefore = List.length parsedSource.file.declarations
                        , declarationsAfter = List.length decodedFile.declarations
                        , commentsBefore = List.length parsedSource.file.comments
                        , commentsAfter = List.length decodedFile.comments
                        }
                }


bytesWidth : Lamdera.Wire3.Bytes -> Int
bytesWidth bytes =
    -- Lamdera.Wire3.Bytes is an alias for Bytes.Bytes, but keeping the helper local
    -- makes the validator code a little easier to read.
    let
        widthHelper : Lamdera.Wire3.Bytes -> Int
        widthHelper rawBytes =
            Bytes.width rawBytes
    in
    widthHelper bytes


firstDiff : String -> String -> Int -> Maybe Int
firstDiff left right index =
    case ( String.uncons left, String.uncons right ) of
        ( Nothing, Nothing ) ->
            Nothing

        ( Nothing, Just _ ) ->
            Just index

        ( Just _, Nothing ) ->
            Just index

        ( Just ( leftChar, leftRest ), Just ( rightChar, rightRest ) ) ->
            if leftChar == rightChar then
                firstDiff leftRest rightRest (index + 1)

            else
                Just index


moduleKind : Elm.Syntax.File.File -> String
moduleKind file =
    case Node.value file.moduleDefinition of
        NormalModule _ ->
            "normal"

        PortModule _ ->
            "port"

        EffectModule _ ->
            "effect"


formatSummary : Int -> ValidationSummary -> String
formatSummary limit summary =
    String.join "\n"
        ([ "Review package AST roundtrip validation"
         , "  total modules: " ++ String.fromInt summary.totalModules
         , "  exact matches: " ++ String.fromInt summary.exactMatches
         , "  json mismatches: " ++ String.fromInt (List.length summary.jsonMismatches)
         , "  decode failures: " ++ String.fromInt (List.length summary.decodeFailures)
         , "  aggregate list decode ok: " ++ boolToString summary.aggregateDecodeOk
         ]
            ++ aggregatePrefixLines summary
            ++ issueLines "decode failures" limit summary.decodeFailures
            ++ issueLines "json mismatches" limit summary.jsonMismatches
        )


aggregatePrefixLines : ValidationSummary -> List String
aggregatePrefixLines summary =
    case ( summary.firstFailingPrefixLength, summary.firstFailingPrefixModule ) of
        ( Just prefixLength, Just moduleName ) ->
            [ "  first failing aggregate prefix length: " ++ String.fromInt prefixLength
            , "  first failing aggregate prefix module: " ++ moduleName
            ]

        _ ->
            []


issueLines : String -> Int -> List ValidationResult -> List String
issueLines label limit results =
    if List.isEmpty results then
        []

    else
        (""
            :: (label ++ ":")
            :: (results
                    |> List.take limit
                    |> List.map formatIssue
               )
        )


formatIssue : ValidationResult -> String
formatIssue result =
    case result.status of
        DecodeFailed ->
            "  - " ++ result.moduleName ++ " decode failed (" ++ String.fromInt result.encodedBytes ++ " bytes)"

        JsonMismatch details ->
            "  - "
                ++ result.moduleName
                ++ " json mismatch"
                ++ " bytes="
                ++ String.fromInt result.encodedBytes
                ++ " diffAt="
                ++ (details.firstDiffIndex |> Maybe.map String.fromInt |> Maybe.withDefault "unknown")
                ++ " kind="
                ++ details.moduleKindBefore
                ++ "->"
                ++ details.moduleKindAfter
                ++ " imports="
                ++ String.fromInt details.importsBefore
                ++ "->"
                ++ String.fromInt details.importsAfter
                ++ " decls="
                ++ String.fromInt details.declarationsBefore
                ++ "->"
                ++ String.fromInt details.declarationsAfter
                ++ " comments="
                ++ String.fromInt details.commentsBefore
                ++ "->"
                ++ String.fromInt details.commentsAfter

        ExactMatch ->
            "  - " ++ result.moduleName ++ " exact match"


boolToString : Bool -> String
boolToString value =
    if value then
        "true"

    else
        "false"
