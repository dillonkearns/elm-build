module SimpleTestRunner exposing (runToString)

{-| Pure-Elm test runner that avoids Json.Encode.

Uses Test.Runner.fromTest to get runners, executes them, and produces
the same CSV+lines format as SimpleSampleTests:

    passCount,failCount,total
    PASS:label
    FAIL:label > with message

-}

import Expect
import Random
import Test exposing (Test)
import Test.Runner


runToString : Test -> String
runToString suite =
    let
        runners : Result String (List Test.Runner.Runner)
        runners =
            case Test.Runner.fromTest 1 (Random.initialSeed 42) suite of
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
            "0,1,1\nFAIL:Invalid test suite: " ++ msg

        Ok runnerList ->
            let
                results : List { passed : Bool, label : String, message : String }
                results =
                    List.map runOneRunner runnerList

                passCount : Int
                passCount =
                    List.length (List.filter .passed results)

                failCount : Int
                failCount =
                    List.length results - passCount

                formatResult : { passed : Bool, label : String, message : String } -> String
                formatResult r =
                    if r.passed then
                        "PASS:" ++ r.label

                    else
                        "FAIL:" ++ r.label ++ " | " ++ r.message
            in
            String.fromInt passCount
                ++ ","
                ++ String.fromInt failCount
                ++ ","
                ++ String.fromInt (List.length results)
                ++ "\n"
                ++ (List.map formatResult results |> String.join "\n")


runOneRunner : Test.Runner.Runner -> { passed : Bool, label : String, message : String }
runOneRunner runner =
    let
        labelPath : String
        labelPath =
            List.reverse runner.labels |> String.join " > "

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
    , label = labelPath
    , message =
        if passed then
            ""

        else
            String.join "; " failures
    }
