module MathLibTests exposing (results, suite)

import Expect
import MathLib
import SimpleTestRunner
import Test exposing (Test, describe, test)


results : String
results =
    SimpleTestRunner.runToString suite


suite : Test
suite =
    describe "MathLib"
        [ describe "abs"
            [ test "positive stays positive" <|
                \_ -> Expect.equal (MathLib.abs 5) 5
            , test "negative becomes positive" <|
                \_ -> Expect.equal (MathLib.abs -3) 3
            , test "zero stays zero" <|
                \_ -> Expect.equal (MathLib.abs 0) 0
            ]
        , describe "sign"
            [ test "positive returns 1" <|
                \_ -> Expect.equal (MathLib.sign 42) 1
            , test "negative returns -1" <|
                \_ -> Expect.equal (MathLib.sign -7) -1
            , test "zero returns 0" <|
                \_ -> Expect.equal (MathLib.sign 0) 0
            ]
        , describe "max"
            [ test "returns larger of two" <|
                \_ -> Expect.equal (MathLib.max 3 7) 7
            , test "returns first when equal" <|
                \_ -> Expect.equal (MathLib.max 5 5) 5
            , test "handles negatives" <|
                \_ -> Expect.equal (MathLib.max -1 -5) -1
            ]
        , describe "isEven"
            [ test "even number" <|
                \_ -> Expect.equal (MathLib.isEven 4) True
            , test "odd number" <|
                \_ -> Expect.equal (MathLib.isEven 3) False
            , test "zero is even" <|
                \_ -> Expect.equal (MathLib.isEven 0) True
            ]
        , describe "clamp"
            [ test "value in range stays" <|
                \_ -> Expect.equal (MathLib.clamp 0 10 5) 5
            , test "value below range clamps to low" <|
                \_ -> Expect.equal (MathLib.clamp 0 10 -3) 0
            , test "value above range clamps to high" <|
                \_ -> Expect.equal (MathLib.clamp 0 10 15) 10
            ]
        ]
