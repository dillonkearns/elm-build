module ValueWireCodecSelfTest exposing (run)

import Array
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import FastDict
import Pages.Script as Script exposing (Script)
import Types exposing (Value(..))
import ValueWireCodec


run : Script
run =
    Script.withoutCliOptions
        (let
            cases =
                [ ( "Int", Int 42 )
                , ( "Float", Float 3.14 )
                , ( "String", String "hello" )
                , ( "Char", Char 'x' )
                , ( "Bool True", Bool True )
                , ( "Bool False", Bool False )
                , ( "Unit", Unit )
                , ( "Tuple", Tuple (Int 1) (String "a") )
                , ( "Triple", Triple (Int 1) (Int 2) (Int 3) )
                , ( "List", List [ Int 1, Int 2, Int 3 ] )
                , ( "empty List", List [] )
                , ( "Record", Record (FastDict.fromList [ ( "x", Int 1 ), ( "y", Int 2 ) ]) )
                , ( "Custom zero-arg", Custom { moduleName = [ "Maybe" ], name = "Nothing" } [] )
                , ( "Custom with args", Custom { moduleName = [ "Maybe" ], name = "Just" } [ Int 42 ] )
                , ( "JsArray", JsArray (Array.fromList [ Int 1, Int 2 ]) )
                ]

            results =
                List.map
                    (\( label, value ) ->
                        case ValueWireCodec.roundTrip value of
                            Just decoded ->
                                if decoded == value then
                                    Ok label

                                else
                                    Err (label ++ ": decoded value differs")

                            Nothing ->
                                Err (label ++ ": decode returned Nothing")
                    )
                    cases

            failures =
                List.filterMap
                    (\r ->
                        case r of
                            Err msg ->
                                Just msg

                            Ok _ ->
                                Nothing
                    )
                    results

            passed =
                List.length cases - List.length failures
         in
         if List.isEmpty failures then
            Do.log ("ValueWireCodec: all " ++ String.fromInt passed ++ " round-trip tests passed") <| \_ ->
            BackendTask.succeed ()

         else
            BackendTask.fail
                (FatalError.fromString
                    ("ValueWireCodec: "
                        ++ String.fromInt (List.length failures)
                        ++ " failures:\n"
                        ++ String.join "\n" failures
                    )
                )
        )
