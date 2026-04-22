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
import Lamdera.Wire3 as Wire


{-| Payload sent once per worker at startup via `Parallel.initShared`.
Mirrors the load-config slice both sides need to call
`InterpreterProject.loadWithPreBuiltGraphs` with matching arguments.

The parallel-worker pool ships this payload via SharedArrayBuffer
(zero-copy across workers — see elm-pages3 commit `300e5c6f`), so heavy
fields ride along without per-worker structured-clone copies.

`depGraph` is small (Dict of dep-name sets), so wire encode/decode is
cheap. Workers feed it into `loadWithPreBuiltGraphs` to skip
`DepGraph.buildGraph` (~500 ms - 1 s on heavy projects).

`baseUserEnvWireBytes` (step 9) optionally carries an encoded
`Eval.Module.WireFields` payload via `ProjectEnvWireCodec`. When
non-empty, workers skip `loadWithPreBuiltGraphs` entirely and decode
the env directly via `ProjectEnvWireCodec.decodeWireFields >>=
Eval.Module.fromWireFields`. Empty `Bytes` (zero-length) means "no
env shipped" — the worker takes the legacy load path for backward
compat with callers that haven't opted in.

A previous experiment that shipped *only* `moduleGraph.moduleToFile`
(the parsed `File` ASTs) regressed elm-review cold by +6 s — per-worker
Wire3 decode of 13.5 MB at ~2.4 MB/s exceeded the build_graph save.
See `.scratch/parallel-ceiling.md` "Tier-2 Option A third attempt".
Step 8a re-measured with range-stripped FunctionImplementation +
RExpr codecs at 6-7 MB/s: ~30× smaller payload, ~3× faster decode.
Step 9 wires that codec into the actual handoff path.

-}
type alias WorkerSharedConfig =
    { projectDir : String
    , sourceDirectories : List String
    , testModuleNames : List String
    , depGraph : DepGraph.Graph
    , baseUserEnvWireBytes : Bytes
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
