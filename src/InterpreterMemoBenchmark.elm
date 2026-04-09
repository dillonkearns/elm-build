module InterpreterMemoBenchmark exposing (run)

{-| Benchmark three memoization cost models on the same pure Elm workload:

- plain evaluation with no memoization
- host-driven hook memoization via `EvYield`
- interpreter-local memoization with cache reuse across invocations

The goal is to validate the runtime-local design and measure whether avoiding
host round-trips improves the constant factors we care about before wiring this
into the review runner.
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
import MemoRuntime
import Pages.Script as Script exposing (Script)
import Path
import Set
import Time
import Types


type alias Config =
    { iterations : Int
    , workScale : Int
    , scenario : Maybe String
    }


type alias ScenarioSpec =
    { name : String
    , inputs : List Int
    }


type alias HookMemoState =
    { cache : FastDict.Dict String Types.Value
    , lookups : Int
    , hits : Int
    , misses : Int
    , stores : Int
    , probeCalls : Int
    }


type alias ScenarioResult =
    { name : String
    , callCount : Int
    , uniqueInputCount : Int
    , probeCalls : Int
    , probeValue : String
    , plainValue : String
    , hookColdValue : String
    , hookWarmValue : String
    , internalColdValue : String
    , internalWarmValue : String
    , plainAvgMs : Float
    , hookColdAvgMs : Float
    , hookWarmAvgMs : Float
    , internalColdAvgMs : Float
    , internalWarmAvgMs : Float
    , hookColdStats : HookMemoState
    , hookWarmStats : HookMemoState
    , internalColdStats : MemoRuntime.MemoStats
    , internalWarmStats : MemoRuntime.MemoStats
    , internalColdEntries : Int
    , internalWarmEntries : Int
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
                                    |> Maybe.withDefault 1
                            )
                        |> Option.withDescription "Benchmark iterations per scenario (default: 1)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "work-scale"
                        |> Option.map
                            (\maybeWorkScale ->
                                maybeWorkScale
                                    |> Maybe.andThen String.toInt
                                    |> Maybe.map (max 1)
                                    |> Maybe.withDefault 1
                            )
                        |> Option.withDescription "Scale scenario call counts linearly (default: 1)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "scenario"
                        |> Option.withDescription "Run only the named scenario (default: all)"
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
        , patchUserSource = \_ source -> source
        , extraSourceFiles = []
        , extraReachableImports = []
        , sourceDirectories = Just []
        }
        |> BackendTask.andThen
            (\project ->
                let
                    scenarios =
                        benchmarkScenarios config.workScale
                            |> filterScenarios config.scenario
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
    -> List ScenarioSpec
    -> BackendTask FatalError (List ScenarioResult)
runScenarios iterations project scenarios =
    case scenarios of
        [] ->
            BackendTask.succeed []

        scenario :: rest ->
            benchmarkScenario iterations project scenario
                |> BackendTask.andThen
                    (\result ->
                        runScenarios iterations project rest
                            |> BackendTask.map (\tail -> result :: tail)
                    )


benchmarkScenario : 
    Int
    -> InterpreterProject.InterpreterProject
    -> ScenarioSpec
    -> BackendTask FatalError ScenarioResult
benchmarkScenario iterations project scenario =
    let
        sources =
            benchmarkSources scenario.inputs
    in
    runProbe project sources emptyHookMemoState
        |> BackendTask.andThen
            (\( probeValue, probePlainState ) ->
                runPlain project sources
                    |> BackendTask.andThen
                        (\plainValue ->
                            runHookMemo project sources emptyHookMemoState
                                |> BackendTask.andThen
                                    (\( hookColdValue, hookPrimedStateWithStats ) ->
                                        let
                                            hookColdSampleState =
                                                hookPrimedStateWithStats

                                            hookPrimedState =
                                                resetHookMemoStats hookPrimedStateWithStats
                                        in
                                        runInternalMemo project sources MemoRuntime.emptyMemoCache
                                            |> BackendTask.andThen
                                                (\internalColdResult ->
                                                    let
                                                        internalPrimedCache =
                                                            internalColdResult.memoCache
                                                    in
                                                    measureAverageMs iterations (\_ -> runPlain project sources |> BackendTask.map (\_ -> ()))
                                                        |> BackendTask.andThen
                                                            (\plainAvgMs ->
                                                                measureAverageMs iterations (\_ -> runHookMemo project sources emptyHookMemoState |> BackendTask.map (\_ -> ()))
                                                                    |> BackendTask.andThen
                                                                    (\hookColdAvgMs ->
                                                                        runHookMemo project sources hookPrimedState
                                                                            |> BackendTask.andThen
                                                                                (\( hookWarmValue, hookWarmSampleState ) ->
                                                                                    measureAverageMs iterations (\_ -> runHookMemo project sources hookPrimedState |> BackendTask.map (\_ -> ()))
                                                                                        |> BackendTask.andThen
                                                                                            (\hookWarmAvgMs ->
                                                                                                measureAverageMs iterations (\_ -> runInternalMemo project sources MemoRuntime.emptyMemoCache |> BackendTask.map (\_ -> ()))
                                                                                                    |> BackendTask.andThen
                                                                                                        (\internalColdAvgMs ->
                                                                                                            runInternalMemo project sources internalPrimedCache
                                                                                                                |> BackendTask.andThen
                                                                                                                    (\internalWarmResult ->
                                                                                                                        measureAverageMs iterations (\_ -> runInternalMemo project sources internalPrimedCache |> BackendTask.map (\_ -> ()))
                                                                                                                            |> BackendTask.map
                                                                                                                                (\internalWarmAvgMs ->
                                                                                                                                    { name = scenario.name
                                                                                                                                    , callCount = List.length scenario.inputs
                                                                                                                                    , uniqueInputCount = uniqueCount scenario.inputs
                                                                                                                                    , probeCalls = probePlainState.probeCalls
                                                                                                                                    , probeValue = valueSummary probeValue
                                                                                                                                    , plainValue = valueSummary plainValue
                                                                                                                                    , hookColdValue = valueSummary hookColdValue
                                                                                                                                    , hookWarmValue = valueSummary hookWarmValue
                                                                                                                                    , internalColdValue = valueSummary internalColdResult.value
                                                                                                                                    , internalWarmValue = valueSummary internalWarmResult.value
                                                                                                                                    , plainAvgMs = plainAvgMs
                                                                                                                                    , hookColdAvgMs = hookColdAvgMs
                                                                                                                                    , hookWarmAvgMs = hookWarmAvgMs
                                                                                                                                    , internalColdAvgMs = internalColdAvgMs
                                                                                                                                    , internalWarmAvgMs = internalWarmAvgMs
                                                                                                                                    , hookColdStats = hookColdSampleState
                                                                                                                                    , hookWarmStats = hookWarmSampleState
                                                                                                                                    , internalColdStats = internalColdResult.memoStats
                                                                                                                                    , internalWarmStats = internalWarmResult.memoStats
                                                                                                                                    , internalColdEntries = memoCacheEntryCount internalColdResult.memoCache
                                                                                                                                    , internalWarmEntries = memoCacheEntryCount internalWarmResult.memoCache
                                                                                                                                    }
                                                                                                                                )
                                                                                                                    )
                                                                                                        )
                                                                                            )
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
    -> HookMemoState
    -> BackendTask FatalError ( Types.Value, HookMemoState )
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


runHookMemo :
    InterpreterProject.InterpreterProject
    -> List String
    -> HookMemoState
    -> BackendTask FatalError ( Types.Value, HookMemoState )
runHookMemo project sources initialState =
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


runInternalMemo :
    InterpreterProject.InterpreterProject
    -> List String
    -> MemoRuntime.MemoCache
    ->
        BackendTask FatalError
            { value : Types.Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
runInternalMemo project sources memoCache =
    case
        InterpreterProject.prepareAndEvalWithMemoizedFunctions project
            { imports = [ "MemoBench" ]
            , expression = "MemoBench.results"
            , sourceOverrides = sources
            , memoizedFunctions = Set.singleton "ExpensiveHelper.expensive"
            , memoCache = memoCache
            , collectMemoStats = True
            }
    of
        Ok result ->
            BackendTask.succeed result

        Err err ->
            BackendTask.fail (FatalError.fromString ("Internal memo benchmark eval failed: " ++ err))


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
    HookMemoState
    -> String
    -> Types.Value
    -> BackendTask FatalError ( HookMemoState, Types.Value )
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


emptyHookMemoState : HookMemoState
emptyHookMemoState =
    { cache = FastDict.empty
    , lookups = 0
    , hits = 0
    , misses = 0
    , stores = 0
    , probeCalls = 0
    }


resetHookMemoStats : HookMemoState -> HookMemoState
resetHookMemoStats state =
    { cache = state.cache
    , lookups = 0
    , hits = 0
    , misses = 0
    , stores = 0
    , probeCalls = 0
    }


measureAverageMs : Int -> (() -> BackendTask FatalError a) -> BackendTask FatalError Float
measureAverageMs iterations taskThunk =
    timeTask (\() -> runRepeatedly iterations taskThunk)
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


timeTask : (() -> BackendTask FatalError a) -> BackendTask FatalError { elapsedMs : Int, value : a }
timeTask taskThunk =
    Do.do BackendTask.Time.now <| \start ->
    Do.do (taskThunk ()) <| \value ->
    Do.do BackendTask.Time.now <| \finish ->
    BackendTask.succeed
        { elapsedMs = Time.posixToMillis finish - Time.posixToMillis start
        , value = value
        }


benchmarkScenarios : Int -> List ScenarioSpec
benchmarkScenarios workScale =
    let
        hotArg =
            1000

        sameArgCount =
            8 * workScale

        mixedUniqueCount =
            4 * workScale

        mixedCopies =
            4

        uniqueCount_ =
            8 * workScale

        sameArgCalls count =
            List.repeat count hotArg

        repeatCycle copies values =
            List.range 1 copies
                |> List.concatMap (\_ -> values)
    in
    [ { name = "single-call"
      , inputs = [ hotArg ]
      }
    , { name = "same-arg-" ++ String.fromInt sameArgCount
      , inputs = sameArgCalls sameArgCount
      }
    , { name = "mixed-" ++ String.fromInt (mixedUniqueCount * mixedCopies) ++ "-" ++ String.fromInt mixedUniqueCount ++ "unique"
      , inputs = repeatCycle mixedCopies (List.range (hotArg - mixedUniqueCount + 1) hotArg)
      }
    , { name = "unique-" ++ String.fromInt uniqueCount_
      , inputs = List.range (hotArg - uniqueCount_ + 1) hotArg
      }
    ]


filterScenarios : Maybe String -> List ScenarioSpec -> List ScenarioSpec
filterScenarios maybeName scenarios =
    case maybeName of
        Nothing ->
            scenarios

        Just name ->
            scenarios
                |> List.filter (\scenario -> scenario.name == name)


benchmarkSources : List Int -> List String
benchmarkSources inputs =
    let
        resultExpression =
            sumExpression "ExpensiveHelper.expensive" inputs

        probeExpression =
            sumExpression "ExpensiveHelper.probe" inputs
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
        ++ probeExpression
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
            , "  shape: calls=" ++ String.fromInt result.callCount ++ ", uniqueInputs=" ++ String.fromInt result.uniqueInputCount
            , "  probe calls: " ++ String.fromInt result.probeCalls
            , "  probe value: " ++ result.probeValue
            , "  values: plain=" ++ result.plainValue ++ ", hookCold=" ++ result.hookColdValue ++ ", hookWarm=" ++ result.hookWarmValue ++ ", internalCold=" ++ result.internalColdValue ++ ", internalWarm=" ++ result.internalWarmValue
            , "  plain avg: " ++ formatMs result.plainAvgMs
            , "  hook cold avg: " ++ formatMs result.hookColdAvgMs
            , "  hook warm avg: " ++ formatMs result.hookWarmAvgMs
            , "  internal cold avg: " ++ formatMs result.internalColdAvgMs
            , "  internal warm avg: " ++ formatMs result.internalWarmAvgMs
            , "  ratios vs plain: hookCold=" ++ formatRatio result.plainAvgMs result.hookColdAvgMs ++ ", hookWarm=" ++ formatRatio result.plainAvgMs result.hookWarmAvgMs ++ ", internalCold=" ++ formatRatio result.plainAvgMs result.internalColdAvgMs ++ ", internalWarm=" ++ formatRatio result.plainAvgMs result.internalWarmAvgMs
            , "  hook cold stats: " ++ formatHookMemoState result.hookColdStats
            , "  hook warm stats: " ++ formatHookMemoState result.hookWarmStats
            , "  internal cold stats: " ++ formatInternalMemoStats result.internalColdEntries result.internalColdStats
            , "  internal warm stats: " ++ formatInternalMemoStats result.internalWarmEntries result.internalWarmStats
            ]
        )


formatHookMemoState : HookMemoState -> String
formatHookMemoState state =
    String.join ", "
        [ "lookups=" ++ String.fromInt state.lookups
        , "hits=" ++ String.fromInt state.hits
        , "misses=" ++ String.fromInt state.misses
        , "stores=" ++ String.fromInt state.stores
        , "hitRate=" ++ formatHitRate state.hits state.lookups
        , "probeCalls=" ++ String.fromInt state.probeCalls
        , "entries=" ++ String.fromInt (state.cache |> FastDict.toList |> List.length)
        ]


formatInternalMemoStats : Int -> MemoRuntime.MemoStats -> String
formatInternalMemoStats entryCount stats =
    MemoRuntime.formatMemoStats entryCount stats


formatMs : Float -> String
formatMs value =
    let
        rounded =
            toFloat (round (value * 1000)) / 1000
    in
    String.fromFloat rounded ++ "ms"


formatRatio : Float -> Float -> String
formatRatio baseline candidate =
    if baseline <= 0 || candidate <= 0 then
        "n/a"

    else
        let
            rounded =
                toFloat (round ((baseline / candidate) * 100)) / 100
        in
        String.fromFloat rounded ++ "x"


formatHitRate : Int -> Int -> String
formatHitRate hits lookups =
    if lookups <= 0 then
        "0%"

    else
        let
            rounded =
                toFloat (round ((toFloat hits / toFloat lookups) * 1000)) / 10
        in
        String.fromFloat rounded ++ "%"


valueSummary : Types.Value -> String
valueSummary value =
    case value of
        Types.Int n ->
            String.fromInt n

        _ ->
            "non-int"


sumExpression : String -> List Int -> String
sumExpression qualifiedName inputs =
    case inputs |> List.map (\input -> qualifiedName ++ " " ++ String.fromInt input) of
        [] ->
            "0"

        calls ->
            """List.foldl (\\value acc -> acc + value) 0
        [ """
                ++ String.join "\n        , " calls
                ++ "\n        ]"


uniqueCount : List Int -> Int
uniqueCount inputs =
    inputs
        |> Set.fromList
        |> Set.size


memoCacheEntryCount : MemoRuntime.MemoCache -> Int
memoCacheEntryCount memoCache =
    MemoRuntime.entryCount memoCache


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
