module TestParallelReal exposing (run)

{-| Validation for elm-wrap's local-dev install of elm-pages3.

If this compiles, it means `import BackendTask.Parallel` is resolving
to the local elm-pages3 checkout (which has the `parallel` branch with
the new module). If it fails, the local-dev registration isn't taking
effect for `elm make` / `lamdera make`.

Note: this module is deliberately separate from TestParallelShim.elm so
the JSON-shim validation keeps working even if the local-dev path is
broken.

-}

import BackendTask exposing (BackendTask)
import BackendTask.Parallel as P
import Bytes
import Bytes.Decode
import Bytes.Encode
import FatalError
import Pages.Script as Script exposing (Script)


run : Script
run =
    (P.run "squareU32"
        (Bytes.Encode.encode (Bytes.Encode.unsignedInt32 Bytes.BE 7))
        (Bytes.Decode.unsignedInt32 Bytes.BE)
        |> BackendTask.andThen
            (\squared ->
                Script.log ("squared via real Parallel API: " ++ String.fromInt squared)
            )
    )
        |> Script.withoutCliOptions
