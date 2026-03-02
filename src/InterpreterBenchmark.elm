module InterpreterBenchmark exposing (run)

{-| Parameterized benchmark harness for elm-interpreter.

Generates Elm source code for various scenarios and measures evaluation
performance via `Eval.Module.evalProject`.

    bunx elm-pages run src/InterpreterBenchmark.elm
    bunx elm-pages run src/InterpreterBenchmark.elm -- --iterations 20
    bunx elm-pages run src/InterpreterBenchmark.elm -- --scenario list-size
    bunx elm-pages run src/InterpreterBenchmark.elm -- --json

-}

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Time
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Elm.Syntax.Expression exposing (Expression(..))
import Eval.Module
import FatalError exposing (FatalError)
import Json.Encode
import Pages.Script as Script exposing (Script)
import Time
import Types


run : Script
run =
    Script.withCliOptions programConfig task


type alias Config =
    { iterations : Maybe String
    , scenarioFilter : Maybe String
    , json : Bool
    }


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.optionalKeywordArg "iterations"
                        |> Option.withDescription "Iterations per scenario (default 10)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "scenario"
                        |> Option.withDescription "Filter to scenarios containing NAME"
                    )
                |> OptionsParser.with
                    (Option.flag "json"
                        |> Option.withDescription "Output JSON instead of table"
                    )
            )



-- SCENARIO DEFINITION


type alias Scenario =
    { name : String
    , source : String
    , expression : Expression
    , iterations : Int
    }


buildScenarios : Int -> String -> BackendTask FatalError (List Scenario)
buildScenarios defaultIter baselineSource =
    let
        baselineScenario : Scenario
        baselineScenario =
            { name = "baseline-9-tests"
            , source = baselineSource
            , expression = FunctionOrValue [] "results"
            , iterations = defaultIter * 5
            }

        testCountScenarios : List Scenario
        testCountScenarios =
            List.map
                (\n ->
                    { name = "test-count-" ++ String.fromInt n
                    , source = testCountSource n
                    , expression = FunctionOrValue [] "result"
                    , iterations = defaultIter
                    }
                )
                [ 5, 10, 20, 50, 100 ]

        listSizeScenarios : List Scenario
        listSizeScenarios =
            List.map
                (\n ->
                    { name = "list-size-" ++ String.fromInt n
                    , source = listSizeSource n
                    , expression = FunctionOrValue [] "result"
                    , iterations = defaultIter
                    }
                )
                [ 10, 50, 100, 200, 500, 1000 ]

        stringRepeatScenarios : List Scenario
        stringRepeatScenarios =
            List.map
                (\n ->
                    { name = "string-repeat-" ++ String.fromInt n
                    , source = stringSizeSource n
                    , expression = FunctionOrValue [] "result"
                    , iterations = defaultIter
                    }
                )
                [ 10, 100, 500, 1000, 5000 ]

        recursionScenarios : List Scenario
        recursionScenarios =
            List.map
                (\n ->
                    { name = "recursion-depth-" ++ String.fromInt n
                    , source = recursionDepthSource n
                    , expression = FunctionOrValue [] "result"
                    , iterations = defaultIter
                    }
                )
                [ 10, 50, 100, 200, 500, 1000 ]

        patternMatchScenarios : List Scenario
        patternMatchScenarios =
            List.map
                (\n ->
                    { name = "pattern-match-sum-" ++ String.fromInt n
                    , source = patternMatchSource n
                    , expression = FunctionOrValue [] "result"
                    , iterations = defaultIter
                    }
                )
                [ 10, 50, 100, 200, 500 ]
    in
    BackendTask.succeed
        ([ baselineScenario ]
            ++ testCountScenarios
            ++ listSizeScenarios
            ++ stringRepeatScenarios
            ++ recursionScenarios
            ++ patternMatchScenarios
        )



-- SOURCE GENERATORS


testCountSource : Int -> String
testCountSource n =
    let
        tests =
            List.range 1 n
                |> List.map
                    (\i ->
                        "            ( \"test-"
                            ++ String.fromInt i
                            ++ "\", "
                            ++ String.fromInt i
                            ++ " + 1 == "
                            ++ String.fromInt (i + 1)
                            ++ " )"
                    )
                |> String.join "\n            , "
    in
    """module BenchTestCount exposing (result)


result : String
result =
    let
        tests =
            [ """ ++ tests ++ """
            ]

        passCount =
            List.length (List.filter Tuple.second tests)
    in
    String.fromInt passCount ++ "/" ++ String.fromInt (List.length tests)
"""


listSizeSource : Int -> String
listSizeSource n =
    """module BenchListSize exposing (result)


result : String
result =
    let
        xs =
            List.range 1 """ ++ String.fromInt n ++ """

        total =
            List.foldl (\\x acc -> x + acc) 0 xs
    in
    String.fromInt total
"""


stringSizeSource : Int -> String
stringSizeSource n =
    """module BenchStringRepeat exposing (result)


result : String
result =
    let
        s =
            String.repeat """ ++ String.fromInt n ++ """ "x"
    in
    String.fromInt (String.length s)
"""


recursionDepthSource : Int -> String
recursionDepthSource n =
    """module BenchRecursion exposing (result)


countDown : Int -> Int
countDown n =
    if n <= 0 then
        0
    else
        countDown (n - 1)


result : String
result =
    String.fromInt (countDown """ ++ String.fromInt n ++ """)
"""


patternMatchSource : Int -> String
patternMatchSource n =
    """module BenchPatternMatch exposing (result)


sumList : List Int -> Int
sumList xs =
    case xs of
        [] ->
            0

        head :: tail ->
            head + sumList tail


result : String
result =
    String.fromInt (sumList (List.range 1 """ ++ String.fromInt n ++ """))
"""



-- RUNNING


type alias ScenarioResult =
    { name : String
    , iterations : Int
    , warmupMs : Int
    , totalMs : Int
    , meanMs : Float
    , status : String
    , detail : String
    }


runScenario : Scenario -> BackendTask FatalError ScenarioResult
runScenario scenario =
    Do.do BackendTask.Time.now <| \warmupStart ->
    let
        warmupResult : Result Types.Error Types.Value
        warmupResult =
            runLoop 2 scenario
    in
    Do.do BackendTask.Time.now <| \warmupEnd ->
    let
        warmupMs : Int
        warmupMs =
            Time.posixToMillis warmupEnd - Time.posixToMillis warmupStart
    in
    case warmupResult of
        Err err ->
            BackendTask.succeed
                { name = scenario.name
                , iterations = scenario.iterations
                , warmupMs = warmupMs
                , totalMs = 0
                , meanMs = 0
                , status = "ERROR"
                , detail = errorToString err
                }

        Ok _ ->
            Do.do BackendTask.Time.now <| \measureStart ->
            let
                measureResult : Result Types.Error Types.Value
                measureResult =
                    runLoop scenario.iterations scenario
            in
            Do.do BackendTask.Time.now <| \measureEnd ->
            let
                totalMs : Int
                totalMs =
                    Time.posixToMillis measureEnd - Time.posixToMillis measureStart

                meanMs : Float
                meanMs =
                    toFloat totalMs / toFloat scenario.iterations
            in
            case measureResult of
                Ok value ->
                    BackendTask.succeed
                        { name = scenario.name
                        , iterations = scenario.iterations
                        , warmupMs = warmupMs
                        , totalMs = totalMs
                        , meanMs = meanMs
                        , status = "OK"
                        , detail = valueToString value
                        }

                Err err ->
                    BackendTask.succeed
                        { name = scenario.name
                        , iterations = scenario.iterations
                        , warmupMs = warmupMs
                        , totalMs = totalMs
                        , meanMs = meanMs
                        , status = "ERROR"
                        , detail = errorToString err
                        }


runLoop : Int -> Scenario -> Result Types.Error Types.Value
runLoop iterations scenario =
    runLoopHelp iterations scenario (Eval.Module.evalProject [ scenario.source ] scenario.expression)


runLoopHelp : Int -> Scenario -> Result Types.Error Types.Value -> Result Types.Error Types.Value
runLoopHelp remaining scenario lastResult =
    if remaining <= 1 then
        lastResult

    else
        case lastResult of
            Err _ ->
                lastResult

            Ok _ ->
                runLoopHelp (remaining - 1)
                    scenario
                    (Eval.Module.evalProject [ scenario.source ] scenario.expression)



-- OUTPUT FORMATTING


valueToString : Types.Value -> String
valueToString value =
    case value of
        Types.String s ->
            if String.length s > 80 then
                String.left 77 s ++ "..."

            else
                s

        Types.Int i ->
            String.fromInt i

        Types.Float f ->
            String.fromFloat f

        Types.Bool True ->
            "True"

        Types.Bool False ->
            "False"

        Types.Unit ->
            "()"

        Types.List items ->
            "[List of " ++ String.fromInt (List.length items) ++ " items]"

        _ ->
            "<value>"


errorToString : Types.Error -> String
errorToString err =
    case err of
        Types.ParsingError deadEnds ->
            "ParsingError: " ++ Debug.toString deadEnds

        Types.EvalError evalErr ->
            "EvalError: " ++ Debug.toString evalErr.error


formatTable : Int -> List ScenarioResult -> String
formatTable defaultIter results =
    let
        header : String
        header =
            "\n=== Interpreter Benchmark ("
                ++ String.fromInt defaultIter
                ++ " iterations) ===\n\n"

        columnHeader : String
        columnHeader =
            padRight 36 "Scenario"
                ++ padRight 8 "Iters"
                ++ padRight 14 "Total(ms)"
                ++ padRight 12 "Mean(ms)"
                ++ "Status"

        separator : String
        separator =
            String.repeat 90 "-"

        rows : List String
        rows =
            List.map formatTableRow results
    in
    header
        ++ columnHeader
        ++ "\n"
        ++ separator
        ++ "\n"
        ++ String.join "\n" rows


formatTableRow : ScenarioResult -> String
formatTableRow r =
    let
        totalStr =
            if r.status == "ERROR" && r.totalMs == 0 then
                "--"

            else
                String.fromInt r.totalMs

        meanStr =
            if r.status == "ERROR" && r.totalMs == 0 then
                "--"

            else
                formatFloat r.meanMs

        statusStr =
            if r.status == "OK" then
                r.status

            else
                r.status ++ ": " ++ String.left 40 r.detail
    in
    padRight 36 r.name
        ++ padRight 8 (String.fromInt r.iterations)
        ++ padRight 14 totalStr
        ++ padRight 12 meanStr
        ++ statusStr


formatFloat : Float -> String
formatFloat f =
    let
        whole =
            floor f

        frac =
            round ((f - toFloat whole) * 100)

        fracStr =
            if frac < 10 then
                "0" ++ String.fromInt frac

            else
                String.fromInt frac
    in
    String.fromInt whole ++ "." ++ fracStr


padRight : Int -> String -> String
padRight n s =
    s ++ String.repeat (max 0 (n - String.length s)) " "


formatJson : Int -> List ScenarioResult -> String
formatJson defaultIter results =
    Json.Encode.encode 2
        (Json.Encode.object
            [ ( "config"
              , Json.Encode.object
                    [ ( "defaultIterations", Json.Encode.int defaultIter )
                    ]
              )
            , ( "results"
              , Json.Encode.list encodeResult results
              )
            ]
        )


encodeResult : ScenarioResult -> Json.Encode.Value
encodeResult r =
    Json.Encode.object
        [ ( "name", Json.Encode.string r.name )
        , ( "iterations", Json.Encode.int r.iterations )
        , ( "warmupMs", Json.Encode.int r.warmupMs )
        , ( "totalMs", Json.Encode.int r.totalMs )
        , ( "meanMs", Json.Encode.float r.meanMs )
        , ( "status", Json.Encode.string r.status )
        , ( "detail", Json.Encode.string r.detail )
        ]


formatJsonLine : ScenarioResult -> String
formatJsonLine r =
    Json.Encode.encode 0 (encodeResult r)



-- MAIN TASK


task : Config -> BackendTask FatalError ()
task config =
    let
        defaultIter : Int
        defaultIter =
            config.iterations
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 10

        scenarioFilter : String -> Bool
        scenarioFilter name =
            case config.scenarioFilter of
                Nothing ->
                    True

                Just filter ->
                    String.contains filter name
    in
    Do.allowFatal (File.rawFile "src/SimpleSampleTests.elm") <| \baselineSource ->
    Do.do (buildScenarios defaultIter baselineSource) <| \allScenarios ->
    let
        scenarios : List Scenario
        scenarios =
            List.filter (\s -> scenarioFilter s.name) allScenarios
    in
    if not config.json then
        Do.log
            (Ansi.Color.fontColor Ansi.Color.brightBlue
                ("\n=== Interpreter Benchmark ("
                    ++ String.fromInt defaultIter
                    ++ " iterations) ===\n"
                )
            )
        <| \_ ->
        Do.log
            (padRight 36 "Scenario"
                ++ padRight 8 "Iters"
                ++ padRight 14 "Total(ms)"
                ++ padRight 12 "Mean(ms)"
                ++ "Status"
            )
        <| \_ ->
        Do.log (String.repeat 90 "-") <| \_ ->
        runScenariosTable scenarios

    else
        Do.log "{" <| \_ ->
        Do.log ("  \"config\": { \"defaultIterations\": " ++ String.fromInt defaultIter ++ " },") <| \_ ->
        Do.log "  \"results\": [" <| \_ ->
        runScenariosJson scenarios []


runScenariosTable : List Scenario -> BackendTask FatalError ()
runScenariosTable scenarios =
    case scenarios of
        [] ->
            Do.noop

        scenario :: rest ->
            Do.do (runScenario scenario) <| \result ->
            let
                color =
                    if result.status == "OK" then
                        Ansi.Color.fontColor Ansi.Color.green

                    else
                        Ansi.Color.fontColor Ansi.Color.red
            in
            Do.log (color (formatTableRow result)) <| \_ ->
            runScenariosTable rest


runScenariosJson : List Scenario -> List ScenarioResult -> BackendTask FatalError ()
runScenariosJson scenarios acc =
    case scenarios of
        [] ->
            Do.log "  ]" <| \_ ->
            Do.log "}" <| \_ ->
            Do.noop

        scenario :: rest ->
            Do.do (runScenario scenario) <| \result ->
            let
                comma =
                    if List.isEmpty rest then
                        ""

                    else
                        ","

                line =
                    "    " ++ formatJsonLine result ++ comma
            in
            Do.log line <| \_ ->
            runScenariosJson rest (acc ++ [ result ])
