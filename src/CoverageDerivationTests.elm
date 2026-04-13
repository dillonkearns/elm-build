module CoverageDerivationTests exposing (suite)

import Coverage
import Elm.Syntax.Range exposing (Range)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Coverage"
        [ describe "relevantRunnerIndices"
            [ test "selects only runners whose coverage overlaps the mutation" <|
                \_ ->
                    let
                        perRunnerCoverage =
                            [ { index = 0, coveredRanges = [ range 1 1 10 50 ] }
                            , { index = 1, coveredRanges = [ range 20 1 30 50 ] }
                            , { index = 2, coveredRanges = [ range 5 1 15 50 ] }
                            ]
                    in
                    Coverage.relevantRunnerIndices perRunnerCoverage (range 8 1 8 10)
                        |> Expect.equal [ 0, 2 ]
            , test "returns empty when no runner covers the mutation" <|
                \_ ->
                    let
                        perRunnerCoverage =
                            [ { index = 0, coveredRanges = [ range 1 1 5 50 ] }
                            , { index = 1, coveredRanges = [ range 10 1 15 50 ] }
                            ]
                    in
                    Coverage.relevantRunnerIndices perRunnerCoverage (range 7 1 7 10)
                        |> Expect.equal []
            , test "returns all runners when all cover the mutation" <|
                \_ ->
                    let
                        perRunnerCoverage =
                            [ { index = 0, coveredRanges = [ range 1 1 50 50 ] }
                            , { index = 1, coveredRanges = [ range 1 1 50 50 ] }
                            , { index = 2, coveredRanges = [ range 1 1 50 50 ] }
                            ]
                    in
                    Coverage.relevantRunnerIndices perRunnerCoverage (range 25 1 25 10)
                        |> Expect.equal [ 0, 1, 2 ]
            , test "handles single runner covering mutation" <|
                \_ ->
                    let
                        perRunnerCoverage =
                            [ { index = 0, coveredRanges = [ range 1 1 5 50 ] }
                            , { index = 1, coveredRanges = [ range 10 1 20 50 ] }
                            , { index = 2, coveredRanges = [ range 30 1 40 50 ] }
                            ]
                    in
                    Coverage.relevantRunnerIndices perRunnerCoverage (range 15 1 15 10)
                        |> Expect.equal [ 1 ]
            , test "handles multi-line mutation spanning multiple runner coverages" <|
                \_ ->
                    let
                        perRunnerCoverage =
                            [ { index = 0, coveredRanges = [ range 1 1 10 50 ] }
                            , { index = 1, coveredRanges = [ range 5 1 20 50 ] }
                            , { index = 2, coveredRanges = [ range 15 1 25 50 ] }
                            ]
                    in
                    -- Mutation at lines 5-7 is contained by runners 0 (1-10) and 1 (5-20)
                    Coverage.relevantRunnerIndices perRunnerCoverage (range 5 1 7 50)
                        |> Expect.equal [ 0, 1 ]
            ]
        , describe "filterRangesToFile"
            [ test "keeps ranges within file line count" <|
                \_ ->
                    Coverage.filterRangesToFile 25 [ range 1 1 5 10, range 10 1 20 10, range 50 1 60 10 ]
                        |> Expect.equal [ range 1 1 5 10, range 10 1 20 10 ]
            , test "removes ranges that extend beyond file" <|
                \_ ->
                    Coverage.filterRangesToFile 25 [ range 1 1 5 10, range 20 1 30 10 ]
                        |> Expect.equal [ range 1 1 5 10 ]
            , test "keeps range exactly at file boundary" <|
                \_ ->
                    Coverage.filterRangesToFile 10 [ range 1 1 10 50 ]
                        |> Expect.equal [ range 1 1 10 50 ]
            , test "removes range starting beyond file" <|
                \_ ->
                    Coverage.filterRangesToFile 10 [ range 15 1 20 10 ]
                        |> Expect.equal []
            , test "empty ranges returns empty" <|
                \_ ->
                    Coverage.filterRangesToFile 100 []
                        |> Expect.equal []
            ]
        , describe "isCovered"
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
