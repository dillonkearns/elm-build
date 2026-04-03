module SimpleTestRunner exposing (countTests, runEach, runIndices, runNth, runToString)

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


{-| Run each test runner individually, returning delimited per-test results.

Decomposes the suite ONCE via Test.Runner.fromTest, then runs each runner
separately. Results are separated by "---TEST_SEP---" so the caller can
split and get per-test results without re-decomposing the suite.

Each test's output is: "1,0,1\nPASS:label" or "0,1,1\nFAIL:label | msg"

-}
runEach : Test -> String
runEach suite =
    case Test.Runner.fromTest 1 (Random.initialSeed 42) suite of
        Test.Runner.Plain list ->
            list |> List.map runOneAsString |> String.join "---TEST_SEP---"

        Test.Runner.Only list ->
            list |> List.map runOneAsString |> String.join "---TEST_SEP---"

        Test.Runner.Skipping list ->
            list |> List.map runOneAsString |> String.join "---TEST_SEP---"

        Test.Runner.Invalid msg ->
            "0,1,1\nFAIL:Invalid test suite: " ++ msg


runOneAsString : Test.Runner.Runner -> String
runOneAsString runner =
    let
        result =
            runOneRunner runner
    in
    if result.passed then
        "1,0,1\nPASS:" ++ result.label

    else
        "0,1,1\nFAIL:" ++ result.label ++ " | " ++ result.message


{-| Run only the runners at the given indices (0-indexed).

Decomposes the suite ONCE via Test.Runner.fromTest, then runs only the
selected runners. This is the key optimization: a mutation that only
affects 3 of 30 runners pays for 1 fromTest + 3 executions instead of 30.

Results are separated by "---TEST_SEP---", same as runEach.

-}
runIndices : List Int -> Test -> String
runIndices indices suite =
    let
        runners =
            getRunners suite

        selectedResults =
            indices
                |> List.filterMap
                    (\i ->
                        runners
                            |> List.drop i
                            |> List.head
                            |> Maybe.map runOneAsString
                    )
    in
    String.join "---TEST_SEP---" selectedResults


{-| Count the number of flat test runners in a test suite.
Returns the count as a string (for interpreter eval compatibility).
-}
countTests : Test -> String
countTests suite =
    case Test.Runner.fromTest 1 (Random.initialSeed 42) suite of
        Test.Runner.Plain list ->
            String.fromInt (List.length list)

        Test.Runner.Only list ->
            String.fromInt (List.length list)

        Test.Runner.Skipping list ->
            String.fromInt (List.length list)

        Test.Runner.Invalid _ ->
            "0"


{-| Run the Nth test runner (0-indexed) from a test suite.
Returns the same CSV+lines format as runToString, but for a single runner.
-}
runNth : Int -> Test -> String
runNth n suite =
    case Test.Runner.fromTest 1 (Random.initialSeed 42) suite of
        Test.Runner.Plain list ->
            runNthFromList n list

        Test.Runner.Only list ->
            runNthFromList n list

        Test.Runner.Skipping list ->
            runNthFromList n list

        Test.Runner.Invalid msg ->
            "0,1,1\nFAIL:Invalid test suite: " ++ msg


runNthFromList : Int -> List Test.Runner.Runner -> String
runNthFromList n runners =
    case List.drop n runners |> List.head of
        Just runner ->
            let
                result =
                    runOneRunner runner
            in
            if result.passed then
                "1,0,1\nPASS:" ++ result.label

            else
                "0,1,1\nFAIL:" ++ result.label ++ " | " ++ result.message

        Nothing ->
            "0,1,1\nFAIL:Test index " ++ String.fromInt n ++ " out of range"
