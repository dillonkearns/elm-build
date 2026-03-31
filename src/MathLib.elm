module MathLib exposing (abs, clamp, isEven, max, sign)

{-| A small math library to serve as mutation testing target.
-}


abs : Int -> Int
abs n =
    if n < 0 then
        negate n

    else
        n


sign : Int -> Int
sign n =
    if n > 0 then
        1

    else if n < 0 then
        -1

    else
        0


max : Int -> Int -> Int
max a b =
    if a > b then
        a

    else
        b


isEven : Int -> Bool
isEven n =
    modBy 2 n == 0


clamp : Int -> Int -> Int -> Int
clamp low high value =
    if value < low then
        low

    else if value > high then
        high

    else
        value
