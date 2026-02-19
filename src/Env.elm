module Env exposing (parallelism)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Json.Encode


parallelism : BackendTask FatalError Int
parallelism =
    Do.command "sh" [ "-c", "nproc --all 2>/dev/null || sysctl -n hw.logicalcpu" ] <| \raw ->
    let
        trimmed : String
        trimmed =
            String.trim raw
    in
    case String.toInt trimmed of
        Nothing ->
            BackendTask.fail (FatalError.fromString ("Invalid nproc output: " ++ Json.Encode.encode 0 (Json.Encode.string trimmed)))

        Just n ->
            BackendTask.succeed n
