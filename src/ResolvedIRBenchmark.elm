module ResolvedIRBenchmark exposing (run)

{-| Micro-benchmark comparing the old string-keyed evaluator with Phase 3's
new resolved-IR evaluator.

Builds a `ProjectEnv` ONCE against a small synthetic fixture, then runs
N iterations of each path against the same expressions. The build-time
cost (parsing, resolving, etc.) is amortized across all iterations, so
the reported per-iteration times isolate the evaluator hot path.

Scenarios:

  - **literal**: `42` — trivial expression with no global references.
    The old path walks to evalExpression and returns an Int; the new
    path parses → resolves → evalR → EvOk. Tests per-eval overhead.

  - **user-decl-chain**: A reference to a user declaration that chains
    through several other user declarations (no core calls). This is
    the clean case for the new evaluator — every reference is an
    `RGlobal` into `resolvedBodies`.

  - **arith**: `1 + 2` — a core operator. Measures the Value.toExpression
    + delegation overhead in the new evaluator vs. direct dispatch in
    the old one.

  - **mixed**: A user decl that calls both user decls and core
    functions. Representative of real Elm code.

Usage:

    npx elm-pages run src/ResolvedIRBenchmark.elm -- --iterations 1000

-}

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Time
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Eval.Module
import FatalError exposing (FatalError)
import Json.Encode
import Pages.Script as Script exposing (Script)
import Time
import Types exposing (EvalErrorKind(..), Value(..))


run : Script
run =
    Script.withCliOptions programConfig task


type alias Config =
    { iterations : Maybe String
    , json : Bool
    }


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.optionalKeywordArg "iterations"
                        |> Option.withDescription "Iterations per scenario (default 1000)"
                    )
                |> OptionsParser.with
                    (Option.flag "json"
                        |> Option.withDescription "Output JSON instead of table"
                    )
            )



-- FIXTURE


fixtureSource : String
fixtureSource =
    """module Fixture exposing (..)


base : Int
base =
    10


doubled : Int
doubled =
    base + base


quadrupled : Int
quadrupled =
    doubled + doubled


chain : Int
chain =
    quadrupled


maybeChain : Int
maybeChain =
    case Just base of
        Just v ->
            v

        Nothing ->
            0


listSum : Int
listSum =
    List.sum [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]


arith : Int
arith =
    1 + 2


userOnly : Int
userOnly =
    let
        x =
            1

        y =
            2

        z =
            3
    in
    x



-- AST WALKER — simulates review-rule workload


type Expr
    = Lit Int
    | Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | IfZero Expr Expr Expr


evalExpr : Expr -> Int
evalExpr expr =
    case expr of
        Lit n ->
            n

        Add a b ->
            evalExpr a + evalExpr b

        Mul a b ->
            evalExpr a * evalExpr b

        Sub a b ->
            evalExpr a - evalExpr b

        IfZero c t f ->
            if evalExpr c == 0 then
                evalExpr t

            else
                evalExpr f


sampleAst : Expr
sampleAst =
    Add
        (Mul (Lit 2) (Lit 3))
        (Sub
            (Mul (Lit 4) (Lit 5))
            (IfZero
                (Sub (Lit 10) (Lit 10))
                (Lit 100)
                (Lit 200)
            )
        )


astResult : Int
astResult =
    evalExpr sampleAst



-- FACTORIAL — self-recursive function


factorial : Int -> Int
factorial n =
    if n == 0 then
        1

    else
        n * factorial (n - 1)


factorial10 : Int
factorial10 =
    factorial 10



-- RECORD WORK


type alias Point =
    { x : Int, y : Int }


origin : Point
origin =
    { x = 0, y = 0 }


moveX : Int -> Point -> Point
moveX dx p =
    { p | x = p.x + dx }


moveY : Int -> Point -> Point
moveY dy p =
    { p | y = p.y + dy }


movedPoint : Point
movedPoint =
    origin
        |> moveX 10
        |> moveY 20
        |> moveX 5


movedPointX : Int
movedPointX =
    movedPoint.x
"""



-- SCENARIOS


type alias Scenario =
    { name : String
    , source : String
    , expression : Expression
    , description : String
    }


scenarios : List Scenario
scenarios =
    [ { name = "literal"
      , source = "42"
      , expression = Integer 42
      , description = "bare integer literal"
      }
    , { name = "user-only-let"
      , source = "Fixture.userOnly"
      , expression = FunctionOrValue [ "Fixture" ] "userOnly"
      , description = "user decl: let with 3 bindings, no core"
      }
    , { name = "user-decl-chain"
      , source = "Fixture.chain"
      , expression = FunctionOrValue [ "Fixture" ] "chain"
      , description = "chain through 4 user decls, each with a + operator"
      }
    , { name = "maybe-case"
      , source = "Fixture.maybeChain"
      , expression = FunctionOrValue [ "Fixture" ] "maybeChain"
      , description = "case on Just, references user decl"
      }
    , { name = "core-arith"
      , source = "Fixture.arith"
      , expression = FunctionOrValue [ "Fixture" ] "arith"
      , description = "core operator + via user decl wrapper"
      }
    , { name = "core-list-sum"
      , source = "Fixture.listSum"
      , expression = FunctionOrValue [ "Fixture" ] "listSum"
      , description = "user decl calls List.sum [1..10]"
      }
    , { name = "ast-walker"
      , source = "Fixture.astResult"
      , expression = FunctionOrValue [ "Fixture" ] "astResult"
      , description = "recursive case walker on a small AST — mimics review rules"
      }
    , { name = "factorial-10"
      , source = "Fixture.factorial10"
      , expression = FunctionOrValue [ "Fixture" ] "factorial10"
      , description = "self-recursive factorial of 10"
      }
    , { name = "record-pipeline"
      , source = "Fixture.movedPointX"
      , expression = FunctionOrValue [ "Fixture" ] "movedPointX"
      , description = "record update pipeline with .x access"
      }
    ]



-- BENCHMARK RUNNER


type alias ScenarioResult =
    { name : String
    , description : String
    , iterations : Int
    , oldPathTotalMs : Int
    , newPathTotalMs : Int
    , oldPathMeanUs : Float
    , newPathMeanUs : Float
    , speedup : Float
    , oldPathDetail : String
    , newPathDetail : String
    , status : String
    }



task : Config -> BackendTask FatalError ()
task config =
    let
        iterations : Int
        iterations =
            config.iterations
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 1000
    in
    case Eval.Module.buildProjectEnv [ fixtureSource ] of
        Err err ->
            Do.log ("failed to build project env: " ++ errorToString err) <| \_ ->
            Do.noop

        Ok projectEnv ->
            if not config.json then
                Do.log
                    (Ansi.Color.fontColor Ansi.Color.brightBlue
                        ("\n=== Resolved-IR Benchmark ("
                            ++ String.fromInt iterations
                            ++ " iterations per scenario) ===\n"
                        )
                    )
                <| \_ ->
                Do.log
                    (padRight 22 "Scenario"
                        ++ padRight 14 "Old total"
                        ++ padRight 14 "New total"
                        ++ padRight 14 "Old mean"
                        ++ padRight 14 "New mean"
                        ++ padRight 12 "Speedup"
                        ++ "Status"
                    )
                <| \_ ->
                Do.log (String.repeat 100 "-") <| \_ ->
                runTable projectEnv iterations scenarios

            else
                Do.log "{" <| \_ ->
                Do.log "  \"results\": [" <| \_ ->
                runJson projectEnv iterations scenarios True


runTable : Eval.Module.ProjectEnv -> Int -> List Scenario -> BackendTask FatalError ()
runTable projectEnv iterations remaining =
    case remaining of
        [] ->
            Do.noop

        scenario :: rest ->
            Do.do (runScenario projectEnv iterations scenario) <| \result ->
            Do.log (formatTableRow result) <| \_ ->
            runTable projectEnv iterations rest


runJson : Eval.Module.ProjectEnv -> Int -> List Scenario -> Bool -> BackendTask FatalError ()
runJson projectEnv iterations remaining isFirst =
    case remaining of
        [] ->
            Do.log "  ]" <| \_ ->
            Do.log "}" <| \_ ->
            Do.noop

        scenario :: rest ->
            Do.do (runScenario projectEnv iterations scenario) <| \result ->
            let
                prefix : String
                prefix =
                    if isFirst then
                        "    "

                    else
                        "    ,"
            in
            Do.log (prefix ++ Json.Encode.encode 0 (encodeResult result)) <| \_ ->
            runJson projectEnv iterations rest False


runScenario : Eval.Module.ProjectEnv -> Int -> Scenario -> BackendTask FatalError ScenarioResult
runScenario projectEnv iterations scenario =
    -- Warmup: 2 runs each path to prime JIT / cache etc.
    let
        _ =
            runOldPathN projectEnv 2 scenario

        _ =
            runNewPathN projectEnv 2 scenario
    in
    Do.do BackendTask.Time.now <| \oldStart ->
    let
        oldFinal : Result Types.Error Types.Value
        oldFinal =
            runOldPathN projectEnv iterations scenario
    in
    Do.do BackendTask.Time.now <| \oldEnd ->
    Do.do BackendTask.Time.now <| \newStart ->
    let
        newFinal : Result Types.Error Types.Value
        newFinal =
            runNewPathN projectEnv iterations scenario
    in
    Do.do BackendTask.Time.now <| \newEnd ->
    let
        oldTotalMs : Int
        oldTotalMs =
            Time.posixToMillis oldEnd - Time.posixToMillis oldStart

        newTotalMs : Int
        newTotalMs =
            Time.posixToMillis newEnd - Time.posixToMillis newStart

        oldMeanUs : Float
        oldMeanUs =
            toFloat oldTotalMs * 1000 / toFloat iterations

        newMeanUs : Float
        newMeanUs =
            toFloat newTotalMs * 1000 / toFloat iterations

        speedup : Float
        speedup =
            if newTotalMs == 0 then
                0

            else
                toFloat oldTotalMs / toFloat newTotalMs

        ( status, oldDetail, newDetail ) =
            case ( oldFinal, newFinal ) of
                ( Ok oldVal, Ok newVal ) ->
                    if valueToString oldVal == valueToString newVal then
                        ( "OK", valueToString oldVal, valueToString newVal )

                    else
                        ( "MISMATCH", valueToString oldVal, valueToString newVal )

                ( Err oldErr, _ ) ->
                    ( "OLD-ERR", errorToString oldErr, "" )

                ( _, Err newErr ) ->
                    ( "NEW-ERR", "", errorToString newErr )
    in
    BackendTask.succeed
        { name = scenario.name
        , description = scenario.description
        , iterations = iterations
        , oldPathTotalMs = oldTotalMs
        , newPathTotalMs = newTotalMs
        , oldPathMeanUs = oldMeanUs
        , newPathMeanUs = newMeanUs
        , speedup = speedup
        , oldPathDetail = oldDetail
        , newPathDetail = newDetail
        , status = status
        }


runOldPathN : Eval.Module.ProjectEnv -> Int -> Scenario -> Result Types.Error Types.Value
runOldPathN projectEnv n scenario =
    if n <= 0 then
        Eval.Module.evalWithEnv projectEnv [] scenario.expression

    else
        let
            result =
                Eval.Module.evalWithEnv projectEnv [] scenario.expression
        in
        case result of
            Err _ ->
                result

            Ok _ ->
                if n <= 1 then
                    result

                else
                    runOldPathN projectEnv (n - 1) scenario


runNewPathN : Eval.Module.ProjectEnv -> Int -> Scenario -> Result Types.Error Types.Value
runNewPathN projectEnv n scenario =
    if n <= 0 then
        Eval.Module.evalWithResolvedIRExpression projectEnv scenario.expression

    else
        let
            result =
                Eval.Module.evalWithResolvedIRExpression projectEnv scenario.expression
        in
        case result of
            Err _ ->
                result

            Ok _ ->
                if n <= 1 then
                    result

                else
                    runNewPathN projectEnv (n - 1) scenario



-- FORMATTING


formatTableRow : ScenarioResult -> String
formatTableRow r =
    let
        color : String -> String
        color =
            if r.status == "OK" && r.speedup > 1.05 then
                Ansi.Color.fontColor Ansi.Color.green

            else if r.status /= "OK" then
                Ansi.Color.fontColor Ansi.Color.red

            else
                identity
    in
    color
        (padRight 22 r.name
            ++ padRight 14 (String.fromInt r.oldPathTotalMs ++ " ms")
            ++ padRight 14 (String.fromInt r.newPathTotalMs ++ " ms")
            ++ padRight 14 (formatFloat r.oldPathMeanUs ++ " µs")
            ++ padRight 14 (formatFloat r.newPathMeanUs ++ " µs")
            ++ padRight 12 (formatFloat r.speedup ++ "x")
            ++ r.status
        )


formatFloat : Float -> String
formatFloat f =
    let
        whole : Int
        whole =
            floor f

        frac : Int
        frac =
            round ((f - toFloat whole) * 100)

        fracStr : String
        fracStr =
            if frac < 10 then
                "0" ++ String.fromInt frac

            else
                String.fromInt frac
    in
    String.fromInt whole ++ "." ++ fracStr


padRight : Int -> String -> String
padRight n s =
    let
        visibleLength : Int
        visibleLength =
            String.length s
    in
    s ++ String.repeat (max 0 (n - visibleLength)) " "


encodeResult : ScenarioResult -> Json.Encode.Value
encodeResult r =
    Json.Encode.object
        [ ( "name", Json.Encode.string r.name )
        , ( "description", Json.Encode.string r.description )
        , ( "iterations", Json.Encode.int r.iterations )
        , ( "oldPathTotalMs", Json.Encode.int r.oldPathTotalMs )
        , ( "newPathTotalMs", Json.Encode.int r.newPathTotalMs )
        , ( "oldPathMeanUs", Json.Encode.float r.oldPathMeanUs )
        , ( "newPathMeanUs", Json.Encode.float r.newPathMeanUs )
        , ( "speedup", Json.Encode.float r.speedup )
        , ( "oldPathDetail", Json.Encode.string r.oldPathDetail )
        , ( "newPathDetail", Json.Encode.string r.newPathDetail )
        , ( "status", Json.Encode.string r.status )
        ]



-- HELPERS


errorToString : Types.Error -> String
errorToString err =
    case err of
        Types.ParsingError _ ->
            "ParsingError"

        Types.EvalError { error } ->
            case error of
                TypeError msg ->
                    "TypeError: " ++ msg

                Unsupported msg ->
                    "Unsupported: " ++ msg

                NameError msg ->
                    "NameError: " ++ msg

                Todo msg ->
                    "Todo: " ++ msg

                TailCall _ ->
                    "TailCall"


valueToString : Value -> String
valueToString value =
    case value of
        Int i ->
            "Int " ++ String.fromInt i

        Float f ->
            "Float " ++ String.fromFloat f

        String s ->
            "String " ++ s

        Char c ->
            "Char " ++ String.fromChar c

        Bool b ->
            if b then
                "True"

            else
                "False"

        Unit ->
            "Unit"

        Tuple _ _ ->
            "Tuple"

        Triple _ _ _ ->
            "Triple"

        Record _ ->
            "Record"

        Custom ref _ ->
            "Custom " ++ ref.name

        PartiallyApplied _ _ _ _ _ _ ->
            "PartiallyApplied"

        JsArray _ ->
            "JsArray"

        List items ->
            "List(" ++ String.fromInt (List.length items) ++ ")"

        JsonValue _ ->
            "JsonValue"

        JsonDecoderValue _ ->
            "JsonDecoderValue"

        RegexValue _ ->
            "RegexValue"

        BytesValue _ ->
            "BytesValue"
