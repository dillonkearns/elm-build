module TestRunnerCommon exposing
    ( WorkerSharedConfig
    , encodeWorkerShared
    , workerSharedDecoder
    )

{-| Wire-format definitions shared between the main `TestRunner` script
and its `TestFileWorker` worker module. Lives in its own module so both
sides can reference the same `WorkerSharedConfig` type without making
`TestRunner` ↔ `TestFileWorker` import each other.

The codec uses Lamdera's auto-derived `w3_encode_WorkerSharedConfig` /
`w3_decode_WorkerSharedConfig` (the lamdera compiler synthesizes them
at compile time for any wire-compatible record type), so adding fields
later just keeps deriving — no hand-written encoder to drift out of
sync between the two sides.

-}

import Bytes exposing (Bytes)
import Bytes.Decode
import Lamdera.Wire3 as Wire


{-| Payload sent once per worker at startup via `Parallel.initShared`.
Mirrors the load-config slice both sides need to call
`InterpreterProject.loadWith` with matching arguments.

The parallel-worker pool ships this payload via SharedArrayBuffer
(zero-copy across workers — see elm-pages3 commit `300e5c6f`), so heavy
fields ride along without per-worker structured-clone copies. But adding
heavy IR (parsed user `File` ASTs via `moduleGraph.moduleToFile`) ran
into a separate ceiling: per-worker Lamdera Wire3 **decode** of the
13.5 MB payload costs 3.4-5.7 s on elm-review — exceeding the ~2 s
`build_graph_ms` save it would unlock. See `.scratch/parallel-ceiling.md`
"Tier-2 Option A third attempt — full SAB pipeline" for the bisect.

The infrastructure (`InterpreterProject.loadWithPreBuiltGraphs`,
elm-pages3 SAB) is in place if a future cheaper codec for `File` ASTs
makes the round-trip cost competitive with `build_graph_ms`.

-}
type alias WorkerSharedConfig =
    { projectDir : String
    , sourceDirectories : List String
    , testModuleNames : List String
    }


encodeWorkerShared : WorkerSharedConfig -> Bytes
encodeWorkerShared config =
    Wire.bytesEncode (w3_encode_WorkerSharedConfig config)


{-| Lamdera's `w3_decode_X` is itself a `Bytes.Decode.Decoder X`
(`Wire.Decoder` is a type alias for `Bytes.Decode.Decoder`), so it
plugs straight into `BackendTask.Parallel.initShared` without wrapping.
-}
workerSharedDecoder : Bytes.Decode.Decoder WorkerSharedConfig
workerSharedDecoder =
    w3_decode_WorkerSharedConfig
