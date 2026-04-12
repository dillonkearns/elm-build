module InterpreterProjectTest exposing (suite)

import Expect
import ProjectRoots
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "ProjectRoots"
        [ rootModuleNamesTests
        ]


{-| Regression: `rootModuleNames` once used `allMods |> Set.diff pkgMods`, which
pipe-expands to `Set.diff pkgMods allMods` = ∅. The root set collapsed to just
`extraReachableImports`, so DFS never walked into package deps reached only via
user code — e.g. `Markdown.InlineParser → Url` in elm-markdown's test suite, so
`Url.elm` was dropped from the module graph and `Url.percentEncode` always
errored. Locks in the correct argument order of `Set.diff`.
-}
rootModuleNamesTests : Test
rootModuleNamesTests =
    describe "computeRootModuleNames"
        [ test "user modules become roots (all mods minus pkg mods)" <|
            \_ ->
                ProjectRoots.computeRootModuleNames
                    { allModuleNames = Set.fromList [ "Main", "App.Foo", "Url", "Dict" ]
                    , pkgModuleNames = Set.fromList [ "Url", "Dict" ]
                    , extraReachableImports = []
                    }
                    |> Expect.equal (Set.fromList [ "Main", "App.Foo" ])
        , test "extraReachableImports union with user modules" <|
            \_ ->
                ProjectRoots.computeRootModuleNames
                    { allModuleNames = Set.fromList [ "Main", "Url" ]
                    , pkgModuleNames = Set.fromList [ "Url" ]
                    , extraReachableImports = [ "Test", "Fuzz" ]
                    }
                    |> Expect.equal (Set.fromList [ "Main", "Test", "Fuzz" ])
        , test "empty user modules still yields extras" <|
            \_ ->
                ProjectRoots.computeRootModuleNames
                    { allModuleNames = Set.fromList [ "Url" ]
                    , pkgModuleNames = Set.fromList [ "Url" ]
                    , extraReachableImports = [ "Test" ]
                    }
                    |> Expect.equal (Set.fromList [ "Test" ])
        , test "no pkg overlap leaves all mods as roots" <|
            \_ ->
                ProjectRoots.computeRootModuleNames
                    { allModuleNames = Set.fromList [ "A", "B", "C" ]
                    , pkgModuleNames = Set.empty
                    , extraReachableImports = []
                    }
                    |> Expect.equal (Set.fromList [ "A", "B", "C" ])
        ]
