module Coverage exposing (extractRanges, filterRangesToFile, isCovered, relevantRunnerIndices)

{-| Coverage analysis for mutation testing.

Extracts covered source ranges from the interpreter's CallTree (produced
by tracing), and checks whether a mutation's range is covered by any test.

-}

import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Rope exposing (Rope)
import Set
import Types exposing (CallTree(..))


{-| Extract all evaluated expression ranges from a CallTree.

Walks the tree recursively to collect every Range that was evaluated.
Returns a flat list (not a Set, since Range isn't comparable in Elm).

-}
extractRanges : Rope CallTree -> List Range
extractRanges trees =
    Rope.toList trees
        |> List.concatMap extractFromNode


extractFromNode : CallTree -> List Range
extractFromNode tree =
    case tree of
        CallNode node ->
            let
                (Node range _) =
                    node.expression
            in
            range :: extractRanges node.children

        CoverageRange range ->
            [ range ]

        CoverageSet packedSet ->
            Set.toList packedSet |> List.map Types.unpackRange


{-| Check if a target range is covered by any range in the covered list.

A range is "covered" if any covered range contains or overlaps with it.
Uses simple containment: the target's start must be >= a covered range's start
AND the target's end must be <= that covered range's end.

-}
isCovered : List Range -> Range -> Bool
isCovered coveredRanges target =
    List.any (\covered -> rangeContains covered target) coveredRanges


{-| Does `outer` contain `inner`?

A range A contains range B if A.start <= B.start and A.end >= B.end.
Uses row-major comparison (row first, then column).

-}
rangeContains : Range -> Range -> Bool
rangeContains outer inner =
    positionLte outer.start inner.start && positionLte inner.end outer.end


positionLte : { row : Int, column : Int } -> { row : Int, column : Int } -> Bool
positionLte a b =
    a.row < b.row || (a.row == b.row && a.column <= b.column)


{-| Filter coverage ranges to only those plausibly from a specific file.

Ranges from other files evaluated during a trace have row/col positions
from those other files. By filtering to ranges where end.row <= fileLineCount,
we discard ranges from files shorter than the mutation target, dramatically
reducing false-positive coverage matches across files.

-}
filterRangesToFile : Int -> List Range -> List Range
filterRangesToFile fileLineCount ranges =
    List.filter (\r -> r.start.row <= fileLineCount && r.end.row <= fileLineCount) ranges


{-| Find which runner indices have coverage overlapping a mutation range.

Given per-runner coverage data and a mutation's source range, returns the
indices of runners whose coverage contains the mutation. Used for selective
test execution: only run runners that could detect the mutation.

-}
relevantRunnerIndices :
    List { a | index : Int, coveredRanges : List Range }
    -> Range
    -> List Int
relevantRunnerIndices perRunnerCoverage mutationRange =
    perRunnerCoverage
        |> List.filterMap
            (\rc ->
                if isCovered rc.coveredRanges mutationRange then
                    Just rc.index

                else
                    Nothing
            )
