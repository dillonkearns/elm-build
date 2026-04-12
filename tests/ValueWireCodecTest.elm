module ValueWireCodecTest exposing (suite)

import Array
import Expect
import FastDict
import Test exposing (Test, describe, test)
import Types exposing (Value(..))
import ValueWireCodec


suite : Test
suite =
    describe "ValueWireCodec round-trip"
        [ roundTrip "Int" (Int 42)
        , roundTrip "Float" (Float 3.14)
        , roundTrip "String" (String "hello")
        , roundTrip "Char" (Char 'x')
        , roundTrip "Bool True" (Bool True)
        , roundTrip "Bool False" (Bool False)
        , roundTrip "Unit" Unit
        , roundTrip "Tuple" (Tuple (Int 1) (String "a"))
        , roundTrip "Triple" (Triple (Int 1) (Int 2) (Int 3))
        , roundTrip "List" (List [ Int 1, Int 2, Int 3 ])
        , roundTrip "empty List" (List [])
        , roundTrip "nested List" (List [ List [ Int 1 ], List [ Int 2 ] ])
        , roundTrip "Record" (Record (FastDict.fromList [ ( "x", Int 1 ), ( "y", Int 2 ) ]))
        , roundTrip "Custom zero-arg" (Custom { moduleName = [ "Maybe" ], name = "Nothing" } [])
        , roundTrip "Custom with args" (Custom { moduleName = [ "Maybe" ], name = "Just" } [ Int 42 ])
        , roundTrip "JsArray" (JsArray (Array.fromList [ Int 1, Int 2 ]))
        , roundTrip "deeply nested"
            (List
                [ Tuple (String "key") (Record (FastDict.fromList [ ( "val", Custom { moduleName = [ "Maybe" ], name = "Just" } [ Int 99 ] ) ]))
                ]
            )
        ]


roundTrip : String -> Value -> Test
roundTrip label value =
    test label <|
        \_ ->
            ValueWireCodec.roundTrip value
                |> Expect.equal (Just value)
