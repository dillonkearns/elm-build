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

The parallel-worker pool now ships this payload via SharedArrayBuffer
(zero-copy across workers — see elm-pages3 commit `300e5c6f` adding
SAB infrastructure to `parallel-worker-pool.js`), so the per-worker
structured-clone copy is gone. Heavy fields can be added here without
the +4 s per-worker copy regression that blocked the earlier
`moduleGraph` shipping experiment.

When adding heavier fields (e.g. `depGraph` + `moduleGraph` to skip
`build_graph_ms` on workers), also plumb the worker side to actually
USE the pre-built data — the SAB infrastructure ships the bytes
cheaply, but workers still need a `loadWithPreBuiltGraphs` entry that
skips `build_graph_ms` based on the pre-built input.

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
