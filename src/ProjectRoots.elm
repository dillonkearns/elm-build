module ProjectRoots exposing (computeRootModuleNames)

import Set exposing (Set)


{-| Compute the root module names for reachability analysis.

Roots = (user modules) ∪ extraReachableImports, where "user modules" are all
module names in the graph minus the known package module names.

Extracted as a pure function so `tests/InterpreterProjectTest.elm` can
exercise it without pulling in `InterpreterProject`'s heavy (lamdera/codecs)
transitive dependencies. The previous inline implementation was
`allModuleNames |> Set.diff pkgModuleNames`, which pipe-expands to
`Set.diff pkgModuleNames allModuleNames` = ∅ — roots collapsed to just
`extraReachableImports`, so DFS never walked from user modules into package
deps reached only via user code (e.g. `Markdown.InlineParser → Url`).

-}
computeRootModuleNames :
    { allModuleNames : Set String
    , pkgModuleNames : Set String
    , extraReachableImports : List String
    }
    -> Set String
computeRootModuleNames { allModuleNames, pkgModuleNames, extraReachableImports } =
    Set.diff allModuleNames pkgModuleNames
        |> Set.union (Set.fromList extraReachableImports)
