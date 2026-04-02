module CoverageTests exposing (suite)

import Coverage
import Elm.Syntax.Range exposing (Range)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Coverage"
        [ describe "isCovered"
            [ test "range fully inside a covered range is covered" <|
                \_ ->
                    let
                        coveredRanges =
                            [ range 3 5 3 20 ]
                    in
                    Coverage.isCovered coveredRanges (range 3 10 3 15)
                        |> Expect.equal True
            , test "range outside all covered ranges is not covered" <|
                \_ ->
                    let
                        coveredRanges =
                            [ range 3 5 3 20 ]
                    in
                    Coverage.isCovered coveredRanges (range 10 1 10 5)
                        |> Expect.equal False
            , test "exact match is covered" <|
                \_ ->
                    let
                        coveredRanges =
                            [ range 5 1 5 10 ]
                    in
                    Coverage.isCovered coveredRanges (range 5 1 5 10)
                        |> Expect.equal True
            , test "overlapping range is covered" <|
                \_ ->
                    let
                        coveredRanges =
                            [ range 5 1 5 20 ]
                    in
                    Coverage.isCovered coveredRanges (range 5 5 5 15)
                        |> Expect.equal True
            , test "empty covered list means not covered" <|
                \_ ->
                    Coverage.isCovered [] (range 1 1 1 10)
                        |> Expect.equal False
            , test "multi-line covered range works" <|
                \_ ->
                    let
                        coveredRanges =
                            [ range 3 1 7 10 ]
                    in
                    Coverage.isCovered coveredRanges (range 5 5 5 15)
                        |> Expect.equal True
            ]
        ]


range : Int -> Int -> Int -> Int -> Range
range startRow startCol endRow endCol =
    { start = { row = startRow, column = startCol }
    , end = { row = endRow, column = endCol }
    }
