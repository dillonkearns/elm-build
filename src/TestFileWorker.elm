module TestFileWorker exposing (run)

{-| Worker-side script for `BackendTask.Parallel.worker`-driven test
evaluation. Runs inside one Node Worker thread; the main script
(`TestRunner.elm`) spawns a pool of these via
`Parallel.worker { workerModule = "TestFileWorker", … }`.

Lifecycle inside each worker:

  1. `initShared` — receive the load configuration (project dir, source
     dirs, test module names, fuzz settings) once at startup.
  2. Load the `InterpreterProject` against that config — same
     `loadWith` setup the main runner does, so the package env, module
     graph, and base user env match. Each worker pays this load cost
     once (in parallel with its peers).
  3. Loop on `receiveTask` — pull per-file eval inputs from the parent,
     run `InterpreterProject.evalSimple` (the cache-free pure-compute
     entry point), and ship the resulting eval-output string back via
     `sendResult`.

The per-task interface is a pure `Bytes` -> `Bytes` round-trip:

  - **input bytes** carry a length-prefixed JSON envelope with
    `imports` / `expression` / `sourceOverrides` (everything
    `evalSimple` needs).
  - **output bytes** carry a length-prefixed JSON envelope with the
    eval result string (which may already start with `"ERROR:"` —
    callers handle that the same way they would for a main-thread
    eval).

This module is **never** invoked directly via `npx elm-pages run`; it's
compiled on-demand by elm-pages3 into
`elm-stuff/elm-pages/workers/TestFileWorker.cjs` the first time
TestRunner calls `Parallel.worker { workerModule = "TestFileWorker", … }`.

-}

import BackendTask exposing (BackendTask)
import BackendTask.Parallel
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Encode as BE
import FatalError exposing (FatalError)
import InterpreterProject exposing (InterpreterProject)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)
import Path
import TestRunner
import TestRunnerCommon exposing (WorkerSharedConfig)


run : Script
run =
    (BackendTask.Parallel.initShared TestRunnerCommon.workerSharedDecoder
        |> BackendTask.andThen loadProjectFromShared
        |> BackendTask.andThen workerLoop
    )
        |> Script.withoutCliOptions


loadProjectFromShared : WorkerSharedConfig -> BackendTask FatalError InterpreterProject
loadProjectFromShared config =
    InterpreterProject.loadWithPreBuiltGraphs
        { projectDir = Path.path config.projectDir
        , skipPackages = TestRunner.kernelPackages
        , patchSource = TestRunner.patchSource
        , patchUserSource = \_ source -> source
        , extraSourceFiles = []
        , extraReachableImports = [ "Test", "Fuzz", "Expect", "Test.Runner" ]
        , sourceDirectories = Just config.sourceDirectories
        , normalizationRoots = Just config.testModuleNames
        , preBuiltDepGraph = Just config.depGraph
        , preBuiltModuleGraph = Nothing
        }



-- ── Per-task loop ──


workerLoop : InterpreterProject -> BackendTask FatalError ()
workerLoop project =
    BackendTask.Parallel.receiveTask taskInputDecoder
        |> BackendTask.andThen (runOneEval project)
        |> BackendTask.andThen
            (\output ->
                BackendTask.Parallel.sendResult (encodeJson (encodeOutput output))
            )
        |> BackendTask.andThen (\_ -> workerLoop project)


type alias TaskInput =
    { imports : List String
    , expression : String
    , sourceOverrides : List String
    }


taskInputDecoder : BD.Decoder TaskInput
taskInputDecoder =
    decodeJson
        (Decode.map3 TaskInput
            (Decode.field "imports" (Decode.list Decode.string))
            (Decode.field "expression" Decode.string)
            (Decode.field "sourceOverrides" (Decode.list Decode.string))
        )


runOneEval : InterpreterProject -> TaskInput -> BackendTask FatalError String
runOneEval project input =
    InterpreterProject.evalSimple project
        { imports = input.imports
        , expression = input.expression
        , sourceOverrides = input.sourceOverrides
        }


encodeOutput : String -> Encode.Value
encodeOutput output =
    Encode.object [ ( "output", Encode.string output ) ]



-- ── Length-prefixed JSON helpers ──
--
-- `BackendTask.Parallel.{initShared,receiveTask}` take a
-- `Bytes.Decode.Decoder`, which can't natively consume "all remaining
-- bytes". A 4-byte big-endian length prefix in front of the JSON UTF-8
-- payload sidesteps that without inventing a richer wire format.


decodeJson : Decode.Decoder a -> BD.Decoder a
decodeJson jsonDecoder =
    BD.unsignedInt32 Bytes.BE
        |> BD.andThen BD.string
        |> BD.andThen
            (\jsonStr ->
                case Decode.decodeString jsonDecoder jsonStr of
                    Ok value ->
                        BD.succeed value

                    Err _ ->
                        BD.fail
            )


encodeJson : Encode.Value -> Bytes
encodeJson value =
    let
        utf8 : Bytes
        utf8 =
            BE.encode (BE.string (Encode.encode 0 value))
    in
    BE.encode
        (BE.sequence
            [ BE.unsignedInt32 Bytes.BE (Bytes.width utf8)
            , BE.bytes utf8
            ]
        )
