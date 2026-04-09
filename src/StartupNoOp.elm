module StartupNoOp exposing (run)

import BackendTask
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withoutCliOptions
        (BackendTask.succeed ()
            |> BackendTask.quiet
        )
