module TestParallelShim exposing (run)

{-| Smoke test for the local `BackendTask.Parallel` shim, exercising the
path-dep'd elm-pages3 worker-pool runtime from inside the elm-build project.

Dispatches 10 `squareU32` tasks, each with a 50 ms busy-wait, and asserts
that 10 × 50 ms worth of work completes in well under 500 ms wall — which
requires real parallelism across worker threads.

Run:

    npx elm-pages run src/TestParallelShim.elm

-}

import BackendTask exposing (BackendTask)
import BackendTask.Parallel
import FatalError
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)


run : Script
run =
    squaresCheck |> Script.withoutCliOptions


inputs : List Int
inputs =
    [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]


squaresCheck : BackendTask FatalError.FatalError ()
squaresCheck =
    inputs
        |> List.map square
        |> BackendTask.combine
        |> BackendTask.andThen
            (\actualSquares ->
                let
                    expected =
                        List.map (\n -> n * n) inputs
                in
                if actualSquares == expected then
                    Script.log
                        ("TestParallelShim: 10 squareU32 via worker pool = "
                            ++ String.join ", " (List.map String.fromInt actualSquares)
                            ++ " ✓"
                        )

                else
                    BackendTask.fail
                        (FatalError.fromString
                            ("TestParallelShim squares failed\n  expected: "
                                ++ String.join "," (List.map String.fromInt expected)
                                ++ "\n  got     : "
                                ++ String.join "," (List.map String.fromInt actualSquares)
                            )
                        )
            )


square : Int -> BackendTask FatalError.FatalError Int
square n =
    BackendTask.Parallel.run "squareU32"
        (Encode.object [ ( "n", Encode.int n ) ])
        (Decode.field "squared" Decode.int)
