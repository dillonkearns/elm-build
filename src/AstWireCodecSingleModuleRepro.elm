module AstWireCodecSingleModuleRepro exposing (run)

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
    , moduleName : String
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
                    (Option.optionalKeywordArg "module"
                        |> Option.map (Maybe.withDefault "Bytes")
                        |> Option.withDescription "Exact module name to reproduce (default: Bytes)"
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
                        case
                            List.filter
                                (\parsedSource -> String.join "." parsedSource.moduleName == config.moduleName)
                                parsedSources
                                |> List.head
                        of
                            Nothing ->
                                BackendTask.fail
                                    (FatalError.fromString ("Could not find module " ++ config.moduleName))

                            Just parsedSource ->
                                parsedSource
                                    |> summarizeModuleRoundtrip
                                    |> formatSummary
                                    |> Script.log
                                    |> BackendTask.allowFatal
            )


type alias Summary =
    { moduleName : String
    , encodedBytes : Int
    , lenientDecodeOk : Bool
    , strictDecodeOk : Bool
    , aggregateSingleDecodeOk : Bool
    , strictSmokeStringOk : Bool
    , strictSmokeIntOk : Bool
    , strictSmokeListOk : Bool
    , moduleDefinitionDecodeOk : Bool
    , importsListDecodeOk : Bool
    , declarationsListDecodeOk : Bool
    , commentsListDecodeOk : Bool
    , firstFailingImportIndex : Maybe Int
    , firstFailingDeclarationIndex : Maybe Int
    , firstFailingCommentIndex : Maybe Int
    , lenientExactJson : Bool
    , strictExactJson : Bool
    , lenientFirstDiff : Maybe Int
    , strictFirstDiff : Maybe Int
    , originalModuleKind : String
    , lenientModuleKind : Maybe String
    , strictModuleKind : Maybe String
    , originalImports : Int
    , lenientImports : Maybe Int
    , strictImports : Maybe Int
    , originalDeclarations : Int
    , lenientDeclarations : Maybe Int
    , strictDeclarations : Maybe Int
    , originalComments : Int
    , lenientComments : Maybe Int
    , strictComments : Maybe Int
    }


summarizeModuleRoundtrip : { a | file : Elm.Syntax.File.File, moduleName : List String } -> Summary
summarizeModuleRoundtrip parsedSource =
    let
        originalFile =
            parsedSource.file

        moduleName =
            String.join "." parsedSource.moduleName

        encodedBytes =
            AstWireCodec.encodeToBytes originalFile

        lenientDecoded =
            AstWireCodec.decodeFromBytes encodedBytes

        strictDecoded =
            Lamdera.Wire3.bytesDecodeStrict AstWireCodec.decodeFile encodedBytes

        moduleDefinitionDecodeOk =
            Lamdera.Wire3.bytesEncode (AstWireCodec.encodeNode AstWireCodec.encodeModule originalFile.moduleDefinition)
                |> Lamdera.Wire3.bytesDecodeStrict (AstWireCodec.decodeNode AstWireCodec.decodeModule)
                |> (/=) Nothing

        importsListDecodeOk =
            Lamdera.Wire3.bytesEncode (Lamdera.Wire3.encodeList (AstWireCodec.encodeNode AstWireCodec.encodeImport) originalFile.imports)
                |> Lamdera.Wire3.bytesDecodeStrict (Lamdera.Wire3.decodeList (AstWireCodec.decodeNode AstWireCodec.decodeImport))
                |> (/=) Nothing

        declarationsListDecodeOk =
            Lamdera.Wire3.bytesEncode (Lamdera.Wire3.encodeList (AstWireCodec.encodeNode AstWireCodec.encodeDeclaration) originalFile.declarations)
                |> Lamdera.Wire3.bytesDecodeStrict (Lamdera.Wire3.decodeList (AstWireCodec.decodeNode AstWireCodec.decodeDeclaration))
                |> (/=) Nothing

        commentsListDecodeOk =
            Lamdera.Wire3.bytesEncode (Lamdera.Wire3.encodeList (AstWireCodec.encodeNode Lamdera.Wire3.encodeString) originalFile.comments)
                |> Lamdera.Wire3.bytesDecodeStrict (Lamdera.Wire3.decodeList (AstWireCodec.decodeNode Lamdera.Wire3.decodeString))
                |> (/=) Nothing

        firstFailingImportIndex =
            findFirstFailingIndex
                originalFile.imports
                (AstWireCodec.encodeNode AstWireCodec.encodeImport)
                (AstWireCodec.decodeNode AstWireCodec.decodeImport)

        firstFailingDeclarationIndex =
            findFirstFailingIndex
                originalFile.declarations
                (AstWireCodec.encodeNode AstWireCodec.encodeDeclaration)
                (AstWireCodec.decodeNode AstWireCodec.decodeDeclaration)

        firstFailingCommentIndex =
            findFirstFailingIndex
                originalFile.comments
                (AstWireCodec.encodeNode Lamdera.Wire3.encodeString)
                (AstWireCodec.decodeNode Lamdera.Wire3.decodeString)

        aggregateSingleDecodeOk =
            Lamdera.Wire3.bytesEncode (Lamdera.Wire3.encodeList AstWireCodec.encodeFile [ originalFile ])
                |> Lamdera.Wire3.bytesDecodeStrict (Lamdera.Wire3.decodeList AstWireCodec.decodeFile)
                |> Maybe.map (\decoded -> List.length decoded == 1)
                |> Maybe.withDefault False

        strictSmokeStringOk =
            Lamdera.Wire3.bytesEncode (Lamdera.Wire3.encodeString "hello")
                |> Lamdera.Wire3.bytesDecodeStrict Lamdera.Wire3.decodeString
                |> (==) (Just "hello")

        strictSmokeIntOk =
            Lamdera.Wire3.bytesEncode (Lamdera.Wire3.encodeInt 42)
                |> Lamdera.Wire3.bytesDecodeStrict Lamdera.Wire3.decodeInt
                |> (==) (Just 42)

        strictSmokeListOk =
            Lamdera.Wire3.bytesEncode (Lamdera.Wire3.encodeList Lamdera.Wire3.encodeString [ "a", "b" ])
                |> Lamdera.Wire3.bytesDecodeStrict (Lamdera.Wire3.decodeList Lamdera.Wire3.decodeString)
                |> (==) (Just [ "a", "b" ])

        originalJson =
            Json.Encode.encode 0 (Elm.Syntax.File.encode originalFile)
    in
    { moduleName = moduleName
    , encodedBytes = Bytes.width encodedBytes
    , lenientDecodeOk = lenientDecoded /= Nothing
    , strictDecodeOk = strictDecoded /= Nothing
    , aggregateSingleDecodeOk = aggregateSingleDecodeOk
    , strictSmokeStringOk = strictSmokeStringOk
    , strictSmokeIntOk = strictSmokeIntOk
    , strictSmokeListOk = strictSmokeListOk
    , moduleDefinitionDecodeOk = moduleDefinitionDecodeOk
    , importsListDecodeOk = importsListDecodeOk
    , declarationsListDecodeOk = declarationsListDecodeOk
    , commentsListDecodeOk = commentsListDecodeOk
    , firstFailingImportIndex = firstFailingImportIndex
    , firstFailingDeclarationIndex = firstFailingDeclarationIndex
    , firstFailingCommentIndex = firstFailingCommentIndex
    , lenientExactJson = exactJson originalJson lenientDecoded
    , strictExactJson = exactJson originalJson strictDecoded
    , lenientFirstDiff = diffIndex originalJson lenientDecoded
    , strictFirstDiff = diffIndex originalJson strictDecoded
    , originalModuleKind = moduleKind originalFile
    , lenientModuleKind = Maybe.map moduleKind lenientDecoded
    , strictModuleKind = Maybe.map moduleKind strictDecoded
    , originalImports = List.length originalFile.imports
    , lenientImports = Maybe.map (.imports >> List.length) lenientDecoded
    , strictImports = Maybe.map (.imports >> List.length) strictDecoded
    , originalDeclarations = List.length originalFile.declarations
    , lenientDeclarations = Maybe.map (.declarations >> List.length) lenientDecoded
    , strictDeclarations = Maybe.map (.declarations >> List.length) strictDecoded
    , originalComments = List.length originalFile.comments
    , lenientComments = Maybe.map (.comments >> List.length) lenientDecoded
    , strictComments = Maybe.map (.comments >> List.length) strictDecoded
    }


exactJson : String -> Maybe Elm.Syntax.File.File -> Bool
exactJson originalJson maybeFile =
    maybeFile
        |> Maybe.map (Elm.Syntax.File.encode >> Json.Encode.encode 0 >> (==) originalJson)
        |> Maybe.withDefault False


diffIndex : String -> Maybe Elm.Syntax.File.File -> Maybe Int
diffIndex originalJson maybeFile =
    maybeFile
        |> Maybe.map (Elm.Syntax.File.encode >> Json.Encode.encode 0)
        |> Maybe.andThen (\decodedJson -> firstDiff originalJson decodedJson 0)


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


formatSummary : Summary -> String
formatSummary summary =
    String.join "\n"
        [ "AstWireCodec single-module repro"
        , "  module: " ++ summary.moduleName
        , "  encoded bytes: " ++ String.fromInt summary.encodedBytes
        , "  lenient decode ok: " ++ boolToString summary.lenientDecodeOk
        , "  strict decode ok: " ++ boolToString summary.strictDecodeOk
        , "  aggregate single-item list decode ok: " ++ boolToString summary.aggregateSingleDecodeOk
        , ""
        , "  strict smoke tests:"
        , "    string=" ++ boolToString summary.strictSmokeStringOk
        , "    int=" ++ boolToString summary.strictSmokeIntOk
        , "    list<string>=" ++ boolToString summary.strictSmokeListOk
        , ""
        , "  field checks:"
        , "    moduleDefinition decode ok=" ++ boolToString summary.moduleDefinitionDecodeOk
        , "    imports list decode ok=" ++ boolToString summary.importsListDecodeOk
        , "    declarations list decode ok=" ++ boolToString summary.declarationsListDecodeOk
        , "    comments list decode ok=" ++ boolToString summary.commentsListDecodeOk
        , "    first failing import index=" ++ maybeInt summary.firstFailingImportIndex
        , "    first failing declaration index=" ++ maybeInt summary.firstFailingDeclarationIndex
        , "    first failing comment index=" ++ maybeInt summary.firstFailingCommentIndex
        , ""
        , "  original:"
        , "    kind=" ++ summary.originalModuleKind
        , "    imports=" ++ String.fromInt summary.originalImports
        , "    decls=" ++ String.fromInt summary.originalDeclarations
        , "    comments=" ++ String.fromInt summary.originalComments
        , ""
        , "  lenient roundtrip:"
        , "    exact json=" ++ boolToString summary.lenientExactJson
        , "    first diff=" ++ maybeInt summary.lenientFirstDiff
        , "    kind=" ++ maybeString summary.lenientModuleKind
        , "    imports=" ++ maybeInt summary.lenientImports
        , "    decls=" ++ maybeInt summary.lenientDeclarations
        , "    comments=" ++ maybeInt summary.lenientComments
        , ""
        , "  strict roundtrip:"
        , "    exact json=" ++ boolToString summary.strictExactJson
        , "    first diff=" ++ maybeInt summary.strictFirstDiff
        , "    kind=" ++ maybeString summary.strictModuleKind
        , "    imports=" ++ maybeInt summary.strictImports
        , "    decls=" ++ maybeInt summary.strictDeclarations
        , "    comments=" ++ maybeInt summary.strictComments
        ]


boolToString : Bool -> String
boolToString value =
    if value then
        "true"

    else
        "false"


maybeString : Maybe String -> String
maybeString maybeValue =
    Maybe.withDefault "n/a" maybeValue


maybeInt : Maybe Int -> String
maybeInt maybeValue =
    maybeValue
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "n/a"


findFirstFailingIndex : List a -> (a -> Lamdera.Wire3.Encoder) -> Lamdera.Wire3.Decoder a -> Maybe Int
findFirstFailingIndex values encoder decoder =
    values
        |> List.indexedMap Tuple.pair
        |> List.filter
            (\( index, value ) ->
                Lamdera.Wire3.bytesEncode (encoder value)
                    |> Lamdera.Wire3.bytesDecodeStrict decoder
                    |> (==) Nothing
                    |> (\failed -> if failed then True else False)
            )
        |> List.head
        |> Maybe.map Tuple.first
