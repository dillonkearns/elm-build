module ReviewRunnerFast exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Env
import FatalError exposing (FatalError)
import Json.Decode
import Pages.Script as Script exposing (Script)
import ReviewRunner


run : Script
run =
    Script.withoutCliOptions
        (loadConfig
            |> BackendTask.andThen ReviewRunner.task
            |> BackendTask.quiet
        )


loadConfig : BackendTask FatalError ReviewRunner.Config
loadConfig =
    BackendTask.Env.get "REVIEW_RUNNER_CONFIG_JSON"
        |> BackendTask.andThen
            (\maybeConfigJson ->
                case maybeConfigJson of
                    Nothing ->
                        BackendTask.succeed ReviewRunner.defaultConfig

                    Just configJson ->
                        configJson
                            |> Json.Decode.decodeString ReviewRunner.configDecoder
                            |> Result.mapError
                                (\error ->
                                    FatalError.fromString
                                        ("Failed to decode REVIEW_RUNNER_CONFIG_JSON: "
                                            ++ Json.Decode.errorToString error
                                        )
                                )
                            |> BackendTask.fromResult
            )
