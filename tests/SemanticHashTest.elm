module SemanticHashTest exposing (suite)

import Expect
import SemanticHash
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "SemanticHash"
        [ importAliasTracking
        ]


importAliasTracking : Test
importAliasTracking =
    describe "dependency tracking through import aliases"
        [ test "a mutation in Foo.Bar.baz is tracked when the caller wrote `import Foo.Bar as Bar` and `Bar.baz`" <|
            \_ ->
                let
                    fooBarSource : String
                    fooBarSource =
                        String.join "\n"
                            [ "module Foo.Bar exposing (baz)"
                            , ""
                            , ""
                            , "baz : Int"
                            , "baz = 1"
                            , ""
                            ]

                    fooBarMutated : String
                    fooBarMutated =
                        String.join "\n"
                            [ "module Foo.Bar exposing (baz)"
                            , ""
                            , ""
                            , "baz : Int"
                            , "baz = 2"
                            , ""
                            ]

                    userSource : String
                    userSource =
                        String.join "\n"
                            [ "module User exposing (use)"
                            , ""
                            , "import Foo.Bar as Bar"
                            , ""
                            , ""
                            , "use : Int"
                            , "use = Bar.baz"
                            , ""
                            ]

                    baselineRaw =
                        SemanticHash.buildRawIndex
                            [ { moduleName = "Foo.Bar", source = fooBarSource }
                            , { moduleName = "User", source = userSource }
                            ]

                    baselineSemantic =
                        SemanticHash.resolveRawIndex baselineRaw

                    mutatedRaw =
                        SemanticHash.replaceModuleInRawIndex baselineRaw
                            { moduleName = "Foo.Bar", source = fooBarMutated }

                    mutatedSemantic =
                        SemanticHash.resolveRawIndexIncremental baselineSemantic mutatedRaw

                    diff =
                        SemanticHash.diffIndices baselineSemantic mutatedSemantic
                in
                diff.changed
                    |> Expect.equal (Set.fromList [ "Foo.Bar.baz", "User.use" ])
        , test "a mutation is tracked when the caller wrote `import Foo exposing (bar)` and referenced `bar` unqualified" <|
            \_ ->
                let
                    fooSource : String
                    fooSource =
                        String.join "\n"
                            [ "module Foo exposing (bar)"
                            , ""
                            , ""
                            , "bar : Int"
                            , "bar = 1"
                            , ""
                            ]

                    fooMutated : String
                    fooMutated =
                        String.join "\n"
                            [ "module Foo exposing (bar)"
                            , ""
                            , ""
                            , "bar : Int"
                            , "bar = 2"
                            , ""
                            ]

                    userSource : String
                    userSource =
                        String.join "\n"
                            [ "module User exposing (use)"
                            , ""
                            , "import Foo exposing (bar)"
                            , ""
                            , ""
                            , "use : Int"
                            , "use = bar"
                            , ""
                            ]

                    baselineRaw =
                        SemanticHash.buildRawIndex
                            [ { moduleName = "Foo", source = fooSource }
                            , { moduleName = "User", source = userSource }
                            ]

                    baselineSemantic =
                        SemanticHash.resolveRawIndex baselineRaw

                    mutatedRaw =
                        SemanticHash.replaceModuleInRawIndex baselineRaw
                            { moduleName = "Foo", source = fooMutated }

                    mutatedSemantic =
                        SemanticHash.resolveRawIndexIncremental baselineSemantic mutatedRaw

                    diff =
                        SemanticHash.diffIndices baselineSemantic mutatedSemantic
                in
                diff.changed
                    |> Expect.equal (Set.fromList [ "Foo.bar", "User.use" ])
        , test "a mutation is tracked when a later `import X.Y as X` shadows `import X` and the caller uses `X.foo`" <|
            \_ ->
                let
                    icalSource : String
                    icalSource =
                        String.join "\n"
                            [ "module Ical exposing (Status(..))"
                            , ""
                            , ""
                            , "type Status = Confirmed | Tentative"
                            , ""
                            ]

                    generatorSource : String
                    generatorSource =
                        String.join "\n"
                            [ "module Ical.Generator exposing (event)"
                            , ""
                            , ""
                            , "event : Int -> Int"
                            , "event x = x + 1"
                            , ""
                            ]

                    generatorMutated : String
                    generatorMutated =
                        String.join "\n"
                            [ "module Ical.Generator exposing (event)"
                            , ""
                            , ""
                            , "event : Int -> Int"
                            , "event x = x + 2"
                            , ""
                            ]

                    testSource : String
                    testSource =
                        String.join "\n"
                            [ "module FeedTests exposing (suite)"
                            , ""
                            , "import Ical exposing (Status(..))"
                            , "import Ical.Generator as Ical"
                            , ""
                            , ""
                            , "suite : Int"
                            , "suite = Ical.event 0"
                            , ""
                            ]

                    baselineRaw =
                        SemanticHash.buildRawIndex
                            [ { moduleName = "Ical", source = icalSource }
                            , { moduleName = "Ical.Generator", source = generatorSource }
                            , { moduleName = "FeedTests", source = testSource }
                            ]

                    baselineSemantic =
                        SemanticHash.resolveRawIndex baselineRaw

                    mutatedRaw =
                        SemanticHash.replaceModuleInRawIndex baselineRaw
                            { moduleName = "Ical.Generator", source = generatorMutated }

                    mutatedSemantic =
                        SemanticHash.resolveRawIndexIncremental baselineSemantic mutatedRaw

                    diff =
                        SemanticHash.diffIndices baselineSemantic mutatedSemantic
                in
                diff.changed
                    |> Expect.equal (Set.fromList [ "Ical.Generator.event", "FeedTests.suite" ])
        ]
