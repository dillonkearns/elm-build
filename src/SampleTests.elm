module SampleTests exposing (suite)

import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Sample Tests"
        [ describe "Math"
            [ test "addition" <|
                \_ -> Expect.equal (1 + 1) 2
            , test "multiplication" <|
                \_ -> Expect.equal (3 * 4) 12
            , test "negative" <|
                \_ -> Expect.equal (negate 5) -5
            ]
        , describe "Strings"
            [ test "reverse" <|
                \_ -> Expect.equal (String.reverse "hello") "olleh"
            , test "length" <|
                \_ -> Expect.equal (String.length "elm") 3
            , test "concat" <|
                \_ -> Expect.equal (String.append "foo" "bar") "foobar"
            ]
        , describe "Lists"
            [ test "head" <|
                \_ -> Expect.equal (List.head [ 1, 2, 3 ]) (Just 1)
            , test "length" <|
                \_ -> Expect.equal (List.length [ 1, 2, 3 ]) 3
            , test "reverse" <|
                \_ -> Expect.equal (List.reverse [ 1, 2, 3 ]) [ 3, 2, 1 ]
            ]
        , describe "Fuzz"
            [ fuzz Fuzz.int "int identity" <|
                \n -> Expect.equal n n
            , fuzz Fuzz.string "string reverse reverse" <|
                \s -> Expect.equal (String.reverse (String.reverse s)) s
            ]
        ]
