module RExprDecodeBenchmark exposing (run)

{-| Step-8a thin-slice experiment: measure Wire3 decode throughput on
real resolved-IR (`Eval.Module.ResolvedProject.bodies`) for the
core-extra project. Compare against the tier-2 baseline of ~2.4 MB/s
(File AST decode) before committing to a full ProjectEnv codec.

    bunx elm-pages run src/RExprDecodeBenchmark.elm

Reports:

  - Body count, encoded byte size
  - Encode time (ms)
  - Decode time per iteration + throughput in MB/s
  - Roundtrip integrity (key-set match)

Decision gate: if decode throughput is at or below the tier-2 ~2.4 MB/s
ceiling, full ProjectEnv shipping will regress like the prior File AST
attempts. Pivot to a different architecture before sinking days into
the rest of the codec.

-}

import AstWireCodec
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Time
import Bytes
import Elm.Syntax.Expression exposing (FunctionImplementation)
import Elm.Syntax.Node as Node
import Eval.Module
import Eval.ResolvedIR as IR
import FastDict
import FatalError exposing (FatalError)
import InterpreterProject
import Lamdera.Wire3 as Wire
import Pages.Script as Script exposing (Script)
import Path
import RExprWireCodec
import Set
import Syntax exposing (fakeNode)
import Time
import Types
import ValueWireCodec


run : Script
run =
    Script.withoutCliOptions task


coreExtraDir : String
coreExtraDir =
    "/tmp/core-extra"


skipPackages : Set.Set String
skipPackages =
    Set.fromList
        [ "elm/json"
        , "elm/regex"
        , "elm/html"
        , "elm/virtual-dom"
        , "elm/bytes"
        , "elm/browser"
        , "elm/http"
        , "elm/file"
        , "elm/url"
        ]


iterations : Int
iterations =
    5


task : BackendTask FatalError ()
task =
    Do.log "\n=== RExpr Wire3 decode-throughput experiment ===\n" <|
        \_ ->
            Do.do BackendTask.Time.now <|
                \loadStart ->
                    Do.do
                        (InterpreterProject.loadWith
                            { projectDir = Path.path coreExtraDir
                            , skipPackages = skipPackages
                            , patchSource = \src -> src
                            , patchUserSource = \_ src -> src
                            , extraSourceFiles = []
                            , extraReachableImports = []
                            , sourceDirectories = Just [ coreExtraDir ++ "/tests" ]
                            , normalizationRoots = Nothing
                            }
                        )
                    <|
                        \project ->
                            Do.do BackendTask.Time.now <|
                                \loadEnd ->
                                    benchOnEnv (InterpreterProject.getPackageEnv project)
                                        (Time.posixToMillis loadEnd - Time.posixToMillis loadStart)


benchOnEnv : Eval.Module.ProjectEnv -> Int -> BackendTask FatalError ()
benchOnEnv env loadMs =
    Do.log ("Loaded ProjectEnv in " ++ String.fromInt loadMs ++ " ms") <|
        \_ ->
            Do.do (benchBodies env) <|
                \_ ->
                    Do.do (benchSharedFunctions env) <|
                        \_ ->
                            benchPrecomputed env


benchBodies : Eval.Module.ProjectEnv -> BackendTask FatalError ()
benchBodies env =
    let
        bodies =
            (Eval.Module.projectEnvResolved env).bodies

        bodyCount =
            FastDict.size bodies
    in
    Do.log ("\n--- Slice 1: ResolvedProject.bodies (resolved IR — user code only) ---") <|
        \_ ->
            Do.log ("count=" ++ String.fromInt bodyCount) <|
                \_ ->
                    Do.do BackendTask.Time.now <|
                        \encStart ->
                            let
                                encoded =
                                    RExprWireCodec.encodeBodies bodies

                                encodedBytes =
                                    Bytes.width encoded
                            in
                            Do.do BackendTask.Time.now <|
                                \encEnd ->
                                    let
                                        encodeMs =
                                            Time.posixToMillis encEnd - Time.posixToMillis encStart
                                    in
                                    Do.log
                                        ("encoded "
                                            ++ formatMB encodedBytes
                                            ++ " in "
                                            ++ String.fromInt encodeMs
                                            ++ " ms"
                                        )
                                    <|
                                        \_ ->
                                            Do.do (decodeIterations (\b -> RExprWireCodec.decodeBodies b /= Nothing) encoded []) <|
                                                \samples ->
                                                    Do.do (reportThroughput encodedBytes samples) <|
                                                        \_ -> roundtripCheck bodies


benchSharedFunctions : Eval.Module.ProjectEnv -> BackendTask FatalError ()
benchSharedFunctions env =
    let
        modules =
            Eval.Module.allModuleFunctions env

        moduleCount =
            List.length modules

        functionCount =
            modules
                |> List.map (\( _, fns ) -> FastDict.size fns)
                |> List.sum

        encoded =
            modules
                |> Wire.encodeList encodeModuleFunctions
                |> Wire.bytesEncode

        encodedBytes =
            Bytes.width encoded
    in
    Do.log ("\n--- Slice 2: env.shared.functions (parsed FunctionImplementation ASTs) ---") <|
        \_ ->
            Do.log ("modules=" ++ String.fromInt moduleCount ++ " fns=" ++ String.fromInt functionCount) <|
                \_ ->
                    Do.log ("encoded " ++ formatMB encodedBytes) <|
                        \_ ->
                            Do.do (decodeIterations (\b -> Wire.bytesDecode (Wire.decodeList decodeModuleFunctions) b /= Nothing) encoded []) <|
                                \samples ->
                                    reportThroughput encodedBytes samples


encodeModuleFunctions : ( String, FastDict.Dict String FunctionImplementation ) -> Wire.Encoder
encodeModuleFunctions ( moduleKey, fns ) =
    Wire.encodeSequenceWithoutLength
        [ Wire.encodeString moduleKey
        , fns
            |> FastDict.toList
            |> Wire.encodeList
                (\( name, impl ) ->
                    Wire.encodeSequenceWithoutLength
                        [ Wire.encodeString name
                        , Wire.encodeString (Node.value impl.name)
                        , Wire.encodeList (AstWireCodec.encodePattern << Node.value) impl.arguments
                        , AstWireCodec.encodeExpression (Node.value impl.expression)
                        ]
                )
        ]


decodeModuleFunctions : Wire.Decoder ( String, FastDict.Dict String FunctionImplementation )
decodeModuleFunctions =
    Wire.succeedDecode (\k entries -> ( k, FastDict.fromList entries ))
        |> Wire.andMapDecode Wire.decodeString
        |> Wire.andMapDecode
            (Wire.decodeList
                (Wire.succeedDecode
                    (\dictKey name args expr ->
                        ( dictKey
                        , { name = fakeNode name
                          , arguments = List.map fakeNode args
                          , expression = fakeNode expr
                          }
                        )
                    )
                    |> Wire.andMapDecode Wire.decodeString
                    |> Wire.andMapDecode Wire.decodeString
                    |> Wire.andMapDecode (Wire.decodeList AstWireCodec.decodePattern)
                    |> Wire.andMapDecode AstWireCodec.decodeExpression
                )
            )


benchPrecomputed : Eval.Module.ProjectEnv -> BackendTask FatalError ()
benchPrecomputed env =
    let
        moduleStats =
            Eval.Module.precomputedValuesByModule env

        totalCount =
            Eval.Module.precomputedValuesCount env

        modulesAndValues : List ( String, FastDict.Dict String Types.Value )
        modulesAndValues =
            moduleStats
                |> List.map
                    (\( moduleKey, _ ) ->
                        ( moduleKey
                        , Eval.Module.getModulePrecomputedValues
                            (String.split "." moduleKey)
                            env
                        )
                    )

        encoded =
            modulesAndValues
                |> Wire.encodeList encodePrecomputedModule
                |> Wire.bytesEncode

        encodedBytes =
            Bytes.width encoded
    in
    Do.log ("\n--- Slice 3: env.shared.precomputedValues ---") <|
        \_ ->
            Do.log ("modules=" ++ String.fromInt (List.length moduleStats) ++ " values=" ++ String.fromInt totalCount) <|
                \_ ->
                    Do.log ("encoded " ++ formatMB encodedBytes ++ " (lossy — ValueWireCodec covers ~12 of ~17 variants)") <|
                        \_ ->
                            Do.do (decodeIterations (\b -> Wire.bytesDecode (Wire.decodeList decodePrecomputedModule) b /= Nothing) encoded []) <|
                                \samples ->
                                    reportThroughput encodedBytes samples


encodePrecomputedModule : ( String, FastDict.Dict String Types.Value ) -> Wire.Encoder
encodePrecomputedModule ( moduleKey, values ) =
    Wire.encodeSequenceWithoutLength
        [ Wire.encodeString moduleKey
        , values
            |> FastDict.toList
            |> Wire.encodeList
                (\( name, value ) ->
                    Wire.encodeSequenceWithoutLength
                        [ Wire.encodeString name
                        , ValueWireCodec.encodeValue value
                        ]
                )
        ]


decodePrecomputedModule : Wire.Decoder ( String, FastDict.Dict String Types.Value )
decodePrecomputedModule =
    Wire.succeedDecode (\k entries -> ( k, FastDict.fromList entries ))
        |> Wire.andMapDecode Wire.decodeString
        |> Wire.andMapDecode
            (Wire.decodeList
                (Wire.succeedDecode Tuple.pair
                    |> Wire.andMapDecode Wire.decodeString
                    |> Wire.andMapDecode ValueWireCodec.decodeValue
                )
            )


decodeIterations :
    (Bytes.Bytes -> Bool)
    -> Bytes.Bytes
    -> List Int
    -> BackendTask FatalError (List Int)
decodeIterations decoder bytes acc =
    if List.length acc >= iterations then
        BackendTask.succeed (List.reverse acc)

    else
        Do.do BackendTask.Time.now <|
            \decStart ->
                let
                    ok =
                        decoder bytes
                in
                Do.do BackendTask.Time.now <|
                    \decEnd ->
                        let
                            ms =
                                Time.posixToMillis decEnd - Time.posixToMillis decStart
                        in
                        if not ok then
                            BackendTask.fail (FatalError.fromString "decode returned Nothing")

                        else
                            decodeIterations decoder bytes (ms :: acc)


reportThroughput : Int -> List Int -> BackendTask FatalError ()
reportThroughput encodedBytes samples =
    let
        sortedSamples =
            List.sort samples

        median =
            sortedSamples
                |> List.drop (List.length sortedSamples // 2)
                |> List.head
                |> Maybe.withDefault 0

        minMs =
            List.minimum samples |> Maybe.withDefault 0

        maxMs =
            List.maximum samples |> Maybe.withDefault 0

        meanMs =
            (toFloat (List.sum samples) / toFloat (List.length samples))
                |> round

        mbPerSec sampleMs =
            if sampleMs == 0 then
                0

            else
                toFloat encodedBytes / 1024 / 1024 / (toFloat sampleMs / 1000)

        medianThroughput =
            mbPerSec median

        bestThroughput =
            mbPerSec minMs
    in
    Do.log
        ("Decode samples (ms): "
            ++ (samples |> List.map String.fromInt |> String.join ", ")
        )
    <|
        \_ ->
            Do.log
                ("min="
                    ++ String.fromInt minMs
                    ++ " median="
                    ++ String.fromInt median
                    ++ " mean="
                    ++ String.fromInt meanMs
                    ++ " max="
                    ++ String.fromInt maxMs
                    ++ " ms"
                )
            <|
                \_ ->
                    Do.log
                        ("Throughput (median): "
                            ++ formatThroughput medianThroughput
                            ++ " (best: "
                            ++ formatThroughput bestThroughput
                            ++ ", tier-2 baseline: 2.4 MB/s)"
                        )
                    <|
                        \_ -> BackendTask.succeed ()


roundtripCheck : FastDict.Dict IR.GlobalId IR.RExpr -> BackendTask FatalError ()
roundtripCheck bodies =
    let
        encoded =
            RExprWireCodec.encodeBodies bodies
    in
    case RExprWireCodec.decodeBodies encoded of
        Nothing ->
            BackendTask.fail (FatalError.fromString "roundtrip: decode returned Nothing")

        Just decoded ->
            let
                origKeys =
                    FastDict.keys bodies

                decodedKeys =
                    FastDict.keys decoded
            in
            if origKeys == decodedKeys then
                Do.log
                    ("Roundtrip OK: "
                        ++ String.fromInt (List.length origKeys)
                        ++ " bodies preserved"
                    )
                <|
                    \_ -> BackendTask.succeed ()

            else
                BackendTask.fail
                    (FatalError.fromString
                        ("roundtrip key mismatch: orig="
                            ++ String.fromInt (List.length origKeys)
                            ++ " decoded="
                            ++ String.fromInt (List.length decodedKeys)
                        )
                    )


formatMB : Int -> String
formatMB bytes =
    let
        mb =
            toFloat bytes / 1024 / 1024
    in
    (mb * 100 |> round |> toFloat |> (\n -> n / 100) |> String.fromFloat) ++ " MB"


formatThroughput : Float -> String
formatThroughput mbPerSec =
    (mbPerSec * 100 |> round |> toFloat |> (\n -> n / 100) |> String.fromFloat) ++ " MB/s"
