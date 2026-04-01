module TrickyTests exposing (..)

import Expect
import Test as T


noAnnotation =
    T.describe "no annotation"
        [ T.test "works" <| \_ -> Expect.equal 1 1
        ]


aliased : T.Test
aliased =
    T.describe "aliased"
        [ T.test "works" <| \_ -> Expect.equal 2 2
        ]


helper : String
helper =
    "not a test"


helperInt : Int
helperInt =
    42


makeTest : String -> T.Test
makeTest name =
    T.describe name []
