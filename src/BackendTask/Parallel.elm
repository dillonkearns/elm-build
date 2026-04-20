module BackendTask.Parallel exposing (run)

{-| Local shim giving elm-build access to a worker-pool dispatcher while we
wait for an elm-pages release that ships `BackendTask.Parallel` natively.

The matching JS-side worker pool (elm-pages3 `generator/src/parallel-worker-pool.js`)
is reachable at runtime via the `file:../elm-pages3` dev-dep in
`package.json`. Until elm-pages 12.2 ships an Elm-level entrypoint, we
route through `BackendTask.Custom.run` — the only public Bytes-capable
(well, JSON-capable) surface in 12.1.2 — and let a thin JS wrapper
(`parallelDispatch` in `custom-backend-task.js`) forward the call into
the worker pool.

The API intentionally uses JSON for per-task payload: for the TestRunner
workload, per-task inputs are small (a test file path, config). Large
shared state (the serialized `ResolvedProject`) belongs on a separate
one-shot transport not baked into this primitive.

-}

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode as Encode


{-| Dispatch a task to the Node Worker-thread pool. `portName` refers to an
exported async function in `custom-backend-task.js`; the call is serviced
by a worker from the long-lived pool rather than the main event loop, so
many concurrent `run` calls actually execute in parallel across cores.
-}
run :
    String
    -> Encode.Value
    -> Decode.Decoder output
    -> BackendTask FatalError output
run portName input decoder =
    BackendTask.Custom.run "parallelDispatch"
        (Encode.object
            [ ( "portName", Encode.string portName )
            , ( "input", input )
            ]
        )
        decoder
        |> BackendTask.allowFatal
