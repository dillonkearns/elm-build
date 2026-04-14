module BackendTask.Extra exposing (combine, combineBy, combineBy_, finally, foldSequence, mapSequence, profiling, sequence, sequence_, timed)

import Array exposing (Array)
import BackendTask exposing (BackendTask)
import BackendTask.Customs
import BackendTask.Do as Do
import BackendTask.Time
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script.Spinner as Spinner
import Rope exposing (Rope)
import Time


timed : String -> String -> BackendTask error a -> BackendTask error a
timed labelBefore labelAfter task =
    if labelBefore == "DISABLED FOR NOW" then
        let
            options : Spinner.Options error ( a, Int )
            options =
                Spinner.options labelBefore
                    |> Spinner.withOnCompletion
                        (\res ->
                            case res of
                                Err _ ->
                                    ( Spinner.Fail, Nothing )

                                Ok ( _, delta ) ->
                                    let
                                        msg : String
                                        msg =
                                            labelAfter ++ " in " ++ String.fromInt delta ++ "ms"
                                    in
                                    ( Spinner.Succeed, Just msg )
                        )

            inner : BackendTask error ( a, Int )
            inner =
                Do.do BackendTask.Time.now <| \before ->
                Do.do task <| \res ->
                Do.do BackendTask.Time.now <| \after ->
                let
                    delta : Int
                    delta =
                        Time.posixToMillis after - Time.posixToMillis before
                in
                BackendTask.succeed ( res, delta )
        in
        Spinner.runTaskWithOptions options inner
            |> BackendTask.map Tuple.first

    else
        Do.do BackendTask.Time.now <| \before ->
        Do.log labelBefore <| \_ ->
        Do.do task <| \res ->
        Do.do BackendTask.Time.now <| \after ->
        let
            delta : Int
            delta =
                Time.posixToMillis after - Time.posixToMillis before
        in
        Do.log (labelAfter ++ " in " ++ String.fromInt delta ++ "ms") <| \_ ->
        BackendTask.succeed res


combineBy :
    Int
    -> List (BackendTask error a)
    -> BackendTask error (List a)
combineBy n list =
    list
        |> List.Extra.greedyGroupsOf n
        |> List.map BackendTask.combine
        |> sequence
        |> BackendTask.map List.concat


combineBy_ :
    Int
    -> List (BackendTask error ())
    -> BackendTask error ()
combineBy_ n list =
    list
        |> List.Extra.greedyGroupsOf n
        |> List.map combineIgnore
        |> sequence_
        |> BackendTask.map (\_ -> ())


combineIgnore : List (BackendTask error a) -> BackendTask error ()
combineIgnore inputs =
    let
        arr : Array (BackendTask error a)
        arr =
            Array.fromList inputs

        go : Int -> Int -> BackendTask error ()
        go fromIncluded toExcluded =
            let
                sliceSize : Int
                sliceSize =
                    toExcluded - fromIncluded
            in
            if sliceSize > 128 then
                let
                    mid : Int
                    mid =
                        fromIncluded + sliceSize // 2
                in
                BackendTask.map2 (\_ _ -> ())
                    (go fromIncluded mid)
                    (go mid toExcluded)

            else
                arr
                    |> Array.slice fromIncluded toExcluded
                    |> Array.toList
                    |> BackendTask.combine
                    |> BackendTask.map (\_ -> ())
    in
    go 0 (Array.length arr)


combine : List (BackendTask error a) -> BackendTask error (List a)
combine inputs =
    let
        arr : Array (BackendTask error a)
        arr =
            Array.fromList inputs

        go : Int -> Int -> BackendTask error (Rope a)
        go fromIncluded toExcluded =
            let
                sliceSize : Int
                sliceSize =
                    toExcluded - fromIncluded
            in
            if sliceSize > 128 then
                let
                    mid : Int
                    mid =
                        fromIncluded + sliceSize // 2
                in
                BackendTask.map2 Rope.appendTo
                    (go fromIncluded mid)
                    (go mid toExcluded)

            else
                arr
                    |> Array.slice fromIncluded toExcluded
                    |> Array.toList
                    |> BackendTask.combine
                    |> BackendTask.map Rope.fromList
    in
    go 0 (Array.length arr)
        |> BackendTask.map Rope.toList


{-| Fold over a list sequentially, threading an accumulator through each step.
Each step runs a BackendTask that receives the current accumulator and item,
and returns the new accumulator. Only one step is materialized at a time,
allowing V8 to GC intermediate state between steps.
-}
foldSequence : (a -> acc -> BackendTask error acc) -> acc -> List a -> BackendTask error acc
foldSequence f acc items =
    case items of
        [] ->
            BackendTask.succeed acc

        item :: rest ->
            f item acc
                |> BackendTask.andThen
                    (\newAcc ->
                        foldSequence f newAcc rest
                    )


{-| Like `sequence`, but constructs each BackendTask lazily — the function `f`
is called inside `andThen` so only one item's BackendTask is materialized at a
time. This allows V8 to GC each item's intermediate state before starting the
next one, preventing memory leaks when processing many items.
-}
mapSequence : (a -> BackendTask error b) -> List a -> BackendTask error (List b)
mapSequence f items =
    mapSequenceHelper f items []


mapSequenceHelper : (a -> BackendTask error b) -> List a -> List b -> BackendTask error (List b)
mapSequenceHelper f items acc =
    case items of
        [] ->
            BackendTask.succeed (List.reverse acc)

        item :: rest ->
            f item
                |> BackendTask.andThen
                    (\result ->
                        mapSequenceHelper f rest (result :: acc)
                    )


sequence : List (BackendTask error a) -> BackendTask error (List a)
sequence inputs =
    let
        arr : Array (BackendTask error a)
        arr =
            Array.fromList inputs

        go : Int -> Int -> BackendTask error (Rope a)
        go fromIncluded toExcluded =
            let
                sliceSize : Int
                sliceSize =
                    toExcluded - fromIncluded
            in
            if sliceSize > 128 then
                let
                    mid : Int
                    mid =
                        fromIncluded + sliceSize // 2
                in
                go fromIncluded mid
                    |> BackendTask.andThen
                        (\b ->
                            BackendTask.map (Rope.appendTo b)
                                (go mid toExcluded)
                        )

            else
                arr
                    |> Array.slice fromIncluded toExcluded
                    |> Array.toList
                    |> BackendTask.sequence
                    |> BackendTask.map Rope.fromList
    in
    go 0 (Array.length arr)
        |> BackendTask.map Rope.toList


sequence_ : List (BackendTask error ()) -> BackendTask error ()
sequence_ inputs =
    let
        arr : Array (BackendTask error ())
        arr =
            Array.fromList inputs

        go : Int -> Int -> BackendTask error ()
        go fromIncluded toExcluded =
            let
                sliceSize : Int
                sliceSize =
                    toExcluded - fromIncluded
            in
            if sliceSize > 128 then
                let
                    mid : Int
                    mid =
                        fromIncluded + sliceSize // 2
                in
                go fromIncluded mid
                    |> BackendTask.andThen
                        (\_ ->
                            go mid toExcluded
                        )

            else
                arr
                    |> Array.slice fromIncluded toExcluded
                    |> Array.toList
                    |> BackendTask.sequence
                    |> BackendTask.map (\_ -> ())
    in
    go 0 (Array.length arr)


profiling : String -> BackendTask FatalError a -> BackendTask FatalError a
profiling label t =
    Do.do (BackendTask.Customs.profile label) <| \_ ->
    Do.do t <| \res ->
    Do.do (BackendTask.Customs.profileEnd label) <| \_ ->
    BackendTask.succeed res


finally : BackendTask e () -> BackendTask e a -> BackendTask e a
finally afterTask task =
    task
        |> BackendTask.onError
            (\e ->
                afterTask |> BackendTask.andThen (\_ -> BackendTask.fail e)
            )
        |> BackendTask.andThen
            (\r ->
                afterTask
                    |> BackendTask.map (\_ -> r)
            )
