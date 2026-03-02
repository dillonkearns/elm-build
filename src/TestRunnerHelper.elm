module TestRunnerHelper exposing (runToJson)

{-| Helper module that runs a Test suite and returns results as a JSON string.

This module is designed to be used via eval — it gets compiled and executed
out-of-process via `elm make` + `node`, and the JSON output is parsed by
the test runner.

-}

import Expect
import Json.Encode
import Random
import Test exposing (Test)
import Test.Runner


{-| Run a test suite and encode the results as a JSON string.

JSON format:

    { "passed": N, "failed": N, "total": N,
      "results": [{ "passed": bool, "labels": [...], "message": "..." }] }

-}
runToJson : Test -> String
runToJson suite =
    let
        seed : Random.Seed
        seed =
            Random.initialSeed 42

        runners : Result String (List Test.Runner.Runner)
        runners =
            case Test.Runner.fromTest 100 seed suite of
                Test.Runner.Plain list ->
                    Ok list

                Test.Runner.Only list ->
                    Ok list

                Test.Runner.Skipping list ->
                    Ok list

                Test.Runner.Invalid msg ->
                    Err msg
    in
    case runners of
        Err msg ->
            Json.Encode.encode 0
                (Json.Encode.object
                    [ ( "passed", Json.Encode.int 0 )
                    , ( "failed", Json.Encode.int 1 )
                    , ( "total", Json.Encode.int 1 )
                    , ( "error", Json.Encode.string msg )
                    , ( "results"
                      , Json.Encode.list identity
                            [ Json.Encode.object
                                [ ( "passed", Json.Encode.bool False )
                                , ( "labels", Json.Encode.list Json.Encode.string [ "Invalid test suite" ] )
                                , ( "message", Json.Encode.string msg )
                                ]
                            ]
                      )
                    ]
                )

        Ok runnerList ->
            let
                results : List { passed : Bool, labels : List String, message : String }
                results =
                    runnerList
                        |> List.map runOneRunner

                passCount : Int
                passCount =
                    List.length (List.filter .passed results)

                failCount : Int
                failCount =
                    List.length results - passCount
            in
            Json.Encode.encode 0
                (Json.Encode.object
                    [ ( "passed", Json.Encode.int passCount )
                    , ( "failed", Json.Encode.int failCount )
                    , ( "total", Json.Encode.int (List.length results) )
                    , ( "results"
                      , Json.Encode.list encodeResult results
                      )
                    ]
                )


runOneRunner : Test.Runner.Runner -> { passed : Bool, labels : List String, message : String }
runOneRunner runner =
    let
        labelPath : List String
        labelPath =
            List.reverse runner.labels

        expectations : List Expect.Expectation
        expectations =
            runner.run ()

        failures : List String
        failures =
            expectations
                |> List.filterMap
                    (\expectation ->
                        Test.Runner.getFailureReason expectation
                            |> Maybe.map .description
                    )

        passed : Bool
        passed =
            List.isEmpty failures
    in
    { passed = passed
    , labels = labelPath
    , message =
        if passed then
            ""

        else
            String.join "\n" failures
    }


encodeResult : { passed : Bool, labels : List String, message : String } -> Json.Encode.Value
encodeResult result =
    Json.Encode.object
        [ ( "passed", Json.Encode.bool result.passed )
        , ( "labels", Json.Encode.list Json.Encode.string result.labels )
        , ( "message", Json.Encode.string result.message )
        ]
