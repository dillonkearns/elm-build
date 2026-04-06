module InterpreterMemoBenchmark exposing (run)

{-| Benchmark the eval-loop-hook memoization shape directly.

This measures a generic memo intercept that:

- uses the interpreter's qualified-function intercept seam
- performs lookup/store via `EvYield`
- keeps the memo table purely in memory in the yield driver

The goal is to answer a narrow question before we invest in a larger runtime
memo refactor:

Is a hook-driven memo layer fast enough when we avoid disk and serialization?
-}

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Time
import Bitwise
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Eval.Expression
import EvalResult
import FatalError exposing (FatalError)
import FastDict
import InterpreterProject
import Pages.Script as Script exposing (Script)
import Path
import Set
import Time
import Types


type alias Config =
    { iterations : Int
    }


type alias MemoState =
    { cache : FastDict.Dict String Types.Value
    , lookups : Int
    , hits : Int
    , misses : Int
    , stores : Int
    , probeCalls : Int
    }


type alias ScenarioResult =
    { name : String
    , probeCalls : Int
    , probeValue : String
    , memoColdLookups : Int
    , plainValue : String
    , memoColdValue : String
    , memoWarmValue : String
    , plainAvgMs : Float
    , memoColdAvgMs : Float
    , memoWarmAvgMs : Float
    , coldStats : MemoState
    , warmStats : MemoState
    }


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.optionalKeywordArg "iterations"
                        |> Option.map
                            (\maybeIterations ->
                                maybeIterations
                                    |> Maybe.andThen String.toInt
                                    |> Maybe.withDefault 5
                            )
                        |> Option.withDescription "Benchmark iterations per scenario (default: 5)"
                    )
            )


run : Script
run =
    Script.withCliOptions programConfig (task >> BackendTask.quiet)


task : Config -> BackendTask FatalError ()
task config =
    InterpreterProject.loadWith
        { projectDir = Path.path "."
        , skipPackages = Set.empty
        , patchSource = identity
        , extraSourceFiles = []
        , sourceDirectories = Just []
        }
        |> BackendTask.andThen
            (\project ->
                let
                    scenarios =
                        [ ( "single-call", 1, 2000 )
                        , ( "repeat-8", 8, 2000 )
                        ]
                in
                scenarios
                    |> runScenarios config.iterations project
                    |> BackendTask.andThen
                        (\results ->
                            results
                                |> List.map logScenario
                                |> sequenceTasks
                                |> BackendTask.map (\_ -> ())
                        )
            )


runScenarios :
    Int
    -> InterpreterProject.InterpreterProject
    -> List ( String, Int, Int )
    -> BackendTask FatalError (List ScenarioResult)
runScenarios iterations project scenarios =
    case scenarios of
        [] ->
            BackendTask.succeed []

        ( name, repeats, n ) :: rest ->
            benchmarkScenario iterations project name repeats n
                |> BackendTask.andThen
                    (\result ->
                        runScenarios iterations project rest
                            |> BackendTask.map (\tail -> result :: tail)
                    )


benchmarkScenario :
    Int
    -> InterpreterProject.InterpreterProject
    -> String
    -> Int
    -> Int
    -> BackendTask FatalError ScenarioResult
benchmarkScenario iterations project name repeats n =
    let
        sources =
            benchmarkSources repeats n
    in
    runProbe project sources emptyMemoState
        |> BackendTask.andThen
            (\( probeValue, probePlainState ) ->
                runPlain project sources
                    |> BackendTask.andThen
                        (\plainValue ->
                            runMemo project sources emptyMemoState
                                |> BackendTask.andThen
                                    (\( memoColdValue, primedStateWithStats ) ->
                                        let
                                            coldSampleState =
                                                primedStateWithStats

                                            primedState =
                                                resetMemoStats primedStateWithStats
                                        in
                                        measureAverageMs iterations (\_ -> runPlain project sources |> BackendTask.map (\_ -> ()))
                                            |> BackendTask.andThen
                                                (\plainAvgMs ->
                                                    measureAverageMs iterations (\_ -> runMemo project sources emptyMemoState |> BackendTask.map (\_ -> ()))
                                                        |> BackendTask.andThen
                                                            (\memoColdAvgMs ->
                                                                runMemo project sources primedState
                                                                    |> BackendTask.andThen
                                                                        (\( memoWarmValue, warmSampleState ) ->
                                                                            measureAverageMs iterations (\_ -> runMemo project sources primedState |> BackendTask.map (\_ -> ()))
                                                                                |> BackendTask.map
                                                                                    (\memoWarmAvgMs ->
                                                                                        { name = name
                                                                                        , probeCalls = probePlainState.probeCalls
                                                                                        , probeValue = valueSummary probeValue
                                                                                        , memoColdLookups = coldSampleState.lookups
                                                                                        , plainValue = valueSummary plainValue
                                                                                        , memoColdValue = valueSummary memoColdValue
                                                                                        , memoWarmValue = valueSummary memoWarmValue
                                                                                        , plainAvgMs = plainAvgMs
                                                                                        , memoColdAvgMs = memoColdAvgMs
                                                                                        , memoWarmAvgMs = memoWarmAvgMs
                                                                                        , coldStats = coldSampleState
                                                                                        , warmStats = warmSampleState
                                                                                        }
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


runPlain : InterpreterProject.InterpreterProject -> List String -> BackendTask FatalError Types.Value
runPlain project sources =
    case
        InterpreterProject.prepareAndEvalRaw project
            { imports = [ "MemoBench" ]
            , expression = "MemoBench.results"
            , sourceOverrides = sources
            }
    of
        Ok value ->
            BackendTask.succeed value

        Err err ->
            BackendTask.fail (FatalError.fromString ("Plain benchmark eval failed: " ++ err))


runProbe :
    InterpreterProject.InterpreterProject
    -> List String
    -> MemoState
    -> BackendTask FatalError ( Types.Value, MemoState )
runProbe project sources initialState =
    InterpreterProject.prepareAndEvalWithYieldState project
        { imports = [ "MemoBench" ]
        , expression = "MemoBench.probeResults"
        , sourceOverrides = sources
        , intercepts = probeIntercepts
        , injectedValues = FastDict.empty
        }
        initialState
        handleMemoYield
        |> BackendTask.andThen
            (\( result, finalState ) ->
                case result of
                    Ok value ->
                        BackendTask.succeed ( value, finalState )

                    Err err ->
                        BackendTask.fail (FatalError.fromString ("Probe benchmark eval failed: " ++ err))
            )


runMemo :
    InterpreterProject.InterpreterProject
    -> List String
    -> MemoState
    -> BackendTask FatalError ( Types.Value, MemoState )
runMemo project sources initialState =
    InterpreterProject.prepareAndEvalWithYieldState project
        { imports = [ "MemoBench" ]
        , expression = "MemoBench.results"
        , sourceOverrides = sources
        , intercepts = memoIntercepts
        , injectedValues = FastDict.empty
        }
        initialState
        handleMemoYield
        |> BackendTask.andThen
            (\( result, finalState ) ->
                case result of
                    Ok value ->
                        BackendTask.succeed ( value, finalState )

                    Err err ->
                        BackendTask.fail (FatalError.fromString ("Memo benchmark eval failed: " ++ err))
            )


probeIntercepts : FastDict.Dict String Types.Intercept
probeIntercepts =
    FastDict.fromList
        [ ( "ExpensiveHelper.probe", probeIntercept ) ]


memoIntercepts : FastDict.Dict String Types.Intercept
memoIntercepts =
    FastDict.fromList
        [ ( "ExpensiveHelper.expensive", memoIntercept ) ]


memoIntercept : Types.Intercept
memoIntercept =
    Types.Intercept
        (\context args _ _ ->
            let
                key =
                    memoKey context.qualifiedName args
            in
            Types.EvYield "memo-lookup" (Types.String key)
                (\lookupResult ->
                    case lookupResult of
                        Types.Custom maybeRef [ cachedValue ] ->
                            if maybeRef.moduleName == [ "Maybe" ] && maybeRef.name == "Just" then
                                Types.EvOk cachedValue

                            else
                                evaluateAndStore key context

                        _ ->
                            evaluateAndStore key context
                )
        )


probeIntercept : Types.Intercept
probeIntercept =
    Types.Intercept
        (\_ args _ _ ->
            Types.EvYield "probe-call" Types.Unit
                (\_ ->
                    case args of
                        [ Types.Int n ] ->
                            Types.EvOk (Types.Int (n + 1))

                        _ ->
                            Types.EvOk Types.Unit
                )
        )


evaluateAndStore : String -> Types.InterceptContext -> Types.EvalResult Types.Value
evaluateAndStore key context =
    context.evaluateOriginal ()
        |> EvalResult.andThen
            (\value ->
                Types.EvYield "memo-store"
                    (Types.Record
                        (FastDict.fromList
                            [ ( "key", Types.String key )
                            , ( "value", value )
                            ]
                        )
                    )
                    (\_ -> Types.EvOk value)
            )


memoKey : String -> List Types.Value -> String
memoKey qualifiedName args =
    qualifiedName
        ++ ":"
        ++ String.fromInt
            (List.foldl
                (\value acc ->
                    Bitwise.xor (acc * 16777619) (Eval.Expression.deepHashValue value)
                )
                2166136261
                args
            )


handleMemoYield :
    MemoState
    -> String
    -> Types.Value
    -> BackendTask FatalError ( MemoState, Types.Value )
handleMemoYield state tag payload =
    case tag of
        "memo-lookup" ->
            case payload of
                Types.String key ->
                    case FastDict.get key state.cache of
                        Just cached ->
                            BackendTask.succeed
                                ( { state | lookups = state.lookups + 1, hits = state.hits + 1 }
                                , maybeJust cached
                                )

                        Nothing ->
                            BackendTask.succeed
                                ( { state | lookups = state.lookups + 1, misses = state.misses + 1 }
                                , maybeNothing
                                )

                _ ->
                    BackendTask.succeed ( state, maybeNothing )

        "memo-store" ->
            case payload of
                Types.Record fields ->
                    case ( FastDict.get "key" fields, FastDict.get "value" fields ) of
                        ( Just (Types.String key), Just value ) ->
                            BackendTask.succeed
                                ( { state
                                    | cache = FastDict.insert key value state.cache
                                    , stores = state.stores + 1
                                  }
                                , Types.Unit
                                )

                        _ ->
                            BackendTask.succeed ( state, Types.Unit )

                _ ->
                    BackendTask.succeed ( state, Types.Unit )

        "probe-call" ->
            BackendTask.succeed
                ( { state | probeCalls = state.probeCalls + 1 }
                , Types.Unit
                )

        _ ->
            BackendTask.succeed ( state, Types.Unit )


maybeJust : Types.Value -> Types.Value
maybeJust value =
    Types.Custom { moduleName = [ "Maybe" ], name = "Just" } [ value ]


maybeNothing : Types.Value
maybeNothing =
    Types.Custom { moduleName = [ "Maybe" ], name = "Nothing" } []


emptyMemoState : MemoState
emptyMemoState =
    { cache = FastDict.empty
    , lookups = 0
    , hits = 0
    , misses = 0
    , stores = 0
    , probeCalls = 0
    }


resetMemoStats : MemoState -> MemoState
resetMemoStats state =
    { cache = state.cache
    , lookups = 0
    , hits = 0
    , misses = 0
    , stores = 0
    , probeCalls = 0
    }


measureAverageMs : Int -> (() -> BackendTask FatalError a) -> BackendTask FatalError Float
measureAverageMs iterations taskThunk =
    timeTask (runRepeatedly iterations taskThunk)
        |> BackendTask.map
            (\{ elapsedMs } ->
                if iterations <= 0 then
                    0

                else
                    toFloat elapsedMs / toFloat iterations
            )


runRepeatedly : Int -> (() -> BackendTask FatalError a) -> BackendTask FatalError ()
runRepeatedly remaining taskThunk =
    if remaining <= 0 then
        BackendTask.succeed ()

    else
        taskThunk ()
            |> BackendTask.andThen
                (\_ ->
                    runRepeatedly (remaining - 1) taskThunk
                )


timeTask : BackendTask FatalError a -> BackendTask FatalError { elapsedMs : Int, value : a }
timeTask taskToTime =
    Do.do BackendTask.Time.now <| \start ->
    Do.do taskToTime <| \value ->
    Do.do BackendTask.Time.now <| \finish ->
    BackendTask.succeed
        { elapsedMs = Time.posixToMillis finish - Time.posixToMillis start
        , value = value
        }


benchmarkSources : Int -> Int -> List String
benchmarkSources repeats n =
    let
        resultExpression =
            List.range 1 repeats
                |> List.map (\_ -> "ExpensiveHelper.expensive " ++ String.fromInt n)
                |> String.join "\n        + "
    in
    [ """module ExpensiveHelper exposing (expensive, probe)

import Dict

probe : Int -> Int
probe n =
    n + 1

expensive : Int -> Int
expensive n =
    let
        dict =
            List.foldl (\\i acc -> Dict.insert (String.fromInt i) i acc) Dict.empty (List.range 1 n)

        mapped =
            Dict.map (\\_ v -> v * 3) dict

        filtered =
            Dict.filter (\\_ v -> modBy 2 v == 0) mapped
    in
    Dict.foldl (\\_ v acc -> acc + v) 0 filtered
"""
    , """module MemoBench exposing (probeResults, results)

import ExpensiveHelper

probeResults : Int
probeResults =
    """
        ++ (List.range 1 repeats
                |> List.map (\_ -> "ExpensiveHelper.probe " ++ String.fromInt n)
                |> String.join "\n        + "
           )
        ++ """

results : Int
results =
    """
        ++ resultExpression
        ++ "\n"
    ]


logScenario : ScenarioResult -> BackendTask FatalError ()
logScenario result =
    Script.log
        (String.join "\n"
            [ "Scenario: " ++ result.name
            , "  probe calls: " ++ String.fromInt result.probeCalls
            , "  probe value: " ++ result.probeValue
            , "  memo cold lookups: " ++ String.fromInt result.memoColdLookups
            , "  values: plain=" ++ result.plainValue ++ ", cold=" ++ result.memoColdValue ++ ", warm=" ++ result.memoWarmValue
            , "  plain avg: " ++ formatMs result.plainAvgMs
            , "  memo cold avg: " ++ formatMs result.memoColdAvgMs
            , "  memo warm avg: " ++ formatMs result.memoWarmAvgMs
            , "  cold stats: " ++ formatMemoState result.coldStats
            , "  warm stats: " ++ formatMemoState result.warmStats
            ]
        )


formatMemoState : MemoState -> String
formatMemoState state =
    String.join ", "
        [ "lookups=" ++ String.fromInt state.lookups
        , "hits=" ++ String.fromInt state.hits
        , "misses=" ++ String.fromInt state.misses
        , "stores=" ++ String.fromInt state.stores
        , "probeCalls=" ++ String.fromInt state.probeCalls
        , "entries=" ++ String.fromInt (state.cache |> FastDict.toList |> List.length)
        ]


formatMs : Float -> String
formatMs value =
    let
        rounded =
            toFloat (round (value * 1000)) / 1000
    in
    String.fromFloat rounded ++ "ms"


valueSummary : Types.Value -> String
valueSummary value =
    case value of
        Types.Int n ->
            String.fromInt n

        _ ->
            "non-int"


sequenceTasks : List (BackendTask FatalError a) -> BackendTask FatalError (List a)
sequenceTasks tasks =
    case tasks of
        [] ->
            BackendTask.succeed []

        task_ :: rest ->
            task_
                |> BackendTask.andThen
                    (\value ->
                        sequenceTasks rest
                            |> BackendTask.map (\tail -> value :: tail)
                    )
