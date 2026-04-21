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

NOTE: Two Tier-2 v1 attempts added pre-built load state here:

  - **(Lamdera-derived)** Adding `depGraph : DepGraph.Graph` and
    `moduleGraph : InterpreterProject.ModuleGraph` regressed
    elm-review cold by +4 s.
  - **(AstWireCodec)** Replacing `moduleGraph.moduleToFile : Dict String File`
    with `moduleToFileBytes : Dict String Bytes` (each Bytes encoded
    via the hand-tuned `AstWireCodec`) didn't help — same +4 s
    regression on elm-review.

Direct measurement (commit reverted; see `.scratch/parallel-ceiling.md`)
showed the wire-encode step alone took **1 s for 13.5 MB** of payload
on elm-review. The data size — not the codec choice — is the
bottleneck. The 2 s of `build_graph_ms` we'd save in workers is
mostly eaten by the wire encode + ship cost.

For now, keep `WorkerSharedConfig` lean. A SharedArrayBuffer-backed
shared payload would let the wire bytes be allocated once and zero-copy
shared with all workers — the right tier-2 architecture once we're
ready to invest. Until then, the Cache.run + worker integration's
warm-run win stands alone.

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
