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
import DepGraph
import InterpreterProject exposing (ModuleGraph)
import Lamdera.Wire3 as Wire


{-| Payload sent once per worker at startup via `Parallel.initShared`.
Mirrors the load-config slice both sides need to call
`InterpreterProject.loadWithPreBuiltGraphs` with matching arguments.

The parallel-worker pool ships this payload via SharedArrayBuffer
(zero-copy across workers — see elm-pages3 commit `300e5c6f`), so heavy
fields like `depGraph` + `moduleGraph` ride along without the +4 s
per-worker structured-clone copy that blocked the earlier shipping
experiment. Workers feed the pre-built graphs into
`loadWithPreBuiltGraphs` (commit `f1648b6`) to skip `build_graph_ms`.

-}
type alias WorkerSharedConfig =
    { projectDir : String
    , sourceDirectories : List String
    , testModuleNames : List String
    , depGraph : DepGraph.Graph
    , moduleGraph : ModuleGraph
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
