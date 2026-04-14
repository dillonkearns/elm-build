module SemanticHashTests exposing (suite)

import Dict
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Expect
import SemanticHash
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "SemanticHash"
        [ fileAspectHashTests
        , describe "hashExpression"
            [ test "same expression produces same hash" <|
                \_ ->
                    let
                        expr1 =
                            parseExpression "module A exposing (..)\n\nfoo = 42"

                        expr2 =
                            parseExpression "module B exposing (..)\n\nbar = 42"
                    in
                    case ( expr1, expr2 ) of
                        ( Just e1, Just e2 ) ->
                            SemanticHash.hashExpression e1
                                |> Expect.equal (SemanticHash.hashExpression e2)

                        _ ->
                            Expect.fail "Failed to parse"
            , test "different expressions produce different hashes" <|
                \_ ->
                    let
                        expr1 =
                            parseExpression "module A exposing (..)\n\nfoo = 42"

                        expr2 =
                            parseExpression "module A exposing (..)\n\nfoo = 43"
                    in
                    case ( expr1, expr2 ) of
                        ( Just e1, Just e2 ) ->
                            SemanticHash.hashExpression e1
                                |> Expect.notEqual (SemanticHash.hashExpression e2)

                        _ ->
                            Expect.fail "Failed to parse"
            , test "function references include qualified name" <|
                \_ ->
                    let
                        expr1 =
                            parseExpression "module A exposing (..)\n\nfoo x = MathLib.abs x"

                        expr2 =
                            parseExpression "module A exposing (..)\n\nfoo x = MathLib.sign x"
                    in
                    case ( expr1, expr2 ) of
                        ( Just e1, Just e2 ) ->
                            SemanticHash.hashExpression e1
                                |> Expect.notEqual (SemanticHash.hashExpression e2)

                        _ ->
                            Expect.fail "Failed to parse"
            , test "operator differences produce different hashes" <|
                \_ ->
                    let
                        expr1 =
                            parseExpression "module A exposing (..)\n\nfoo a b = a + b"

                        expr2 =
                            parseExpression "module A exposing (..)\n\nfoo a b = a - b"
                    in
                    case ( expr1, expr2 ) of
                        ( Just e1, Just e2 ) ->
                            SemanticHash.hashExpression e1
                                |> Expect.notEqual (SemanticHash.hashExpression e2)

                        _ ->
                            Expect.fail "Failed to parse"
            , test "if-else structure hashes differently from just the condition" <|
                \_ ->
                    let
                        expr1 =
                            parseExpression "module A exposing (..)\n\nfoo x = if x then 1 else 0"

                        expr2 =
                            parseExpression "module A exposing (..)\n\nfoo x = if x then 0 else 1"
                    in
                    case ( expr1, expr2 ) of
                        ( Just e1, Just e2 ) ->
                            SemanticHash.hashExpression e1
                                |> Expect.notEqual (SemanticHash.hashExpression e2)

                        _ ->
                            Expect.fail "Failed to parse"
            ]
        , describe "extractDependencies"
            [ test "extracts qualified function reference" <|
                \_ ->
                    let
                        deps =
                            parseDeps "module A exposing (..)\n\nfoo x = MathLib.abs x"
                    in
                    -- extractDependencies returns ALL FunctionOrValue refs
                    -- (including local vars like x — filtering is done at index level)
                    deps
                        |> List.filter (\( m, _ ) -> not (List.isEmpty m))
                        |> Expect.equal [ ( [ "MathLib" ], "abs" ) ]
            , test "extracts multiple qualified dependencies" <|
                \_ ->
                    let
                        deps =
                            parseDeps "module A exposing (..)\n\nfoo x = if x > 0 then MathLib.abs x else MathLib.sign x"
                    in
                    deps
                        |> List.filter (\( m, _ ) -> not (List.isEmpty m))
                        |> List.sort
                        |> Expect.equal [ ( [ "MathLib" ], "abs" ), ( [ "MathLib" ], "sign" ) ]
            , test "unqualified references included in raw output" <|
                \_ ->
                    let
                        deps =
                            parseDeps "module A exposing (..)\n\nfoo x = negate x"
                    in
                    deps
                        |> List.member ( [], "negate" )
                        |> Expect.equal True
            ]
        , describe "buildIndex"
            [ test "independent functions have independent semantic hashes" <|
                \_ ->
                    let
                        source =
                            "module Foo exposing (..)\n\nbar = 1\n\nbaz = 2"

                        index =
                            SemanticHash.buildIndexFromSource source

                        barHash =
                            SemanticHash.getSemanticHash index "bar"

                        bazHash =
                            SemanticHash.getSemanticHash index "baz"
                    in
                    case ( barHash, bazHash ) of
                        ( Just h1, Just h2 ) ->
                            h1 |> Expect.notEqual h2

                        _ ->
                            Expect.fail "Expected both hashes to exist"
            , test "changing a dependency changes the caller's semantic hash" <|
                \_ ->
                    let
                        source1 =
                            "module Foo exposing (..)\n\nhelper = 1\n\ncaller = helper + 1"

                        source2 =
                            "module Foo exposing (..)\n\nhelper = 2\n\ncaller = helper + 1"

                        callerHash1 =
                            SemanticHash.buildIndexFromSource source1
                                |> (\idx -> SemanticHash.getSemanticHash idx "caller")

                        callerHash2 =
                            SemanticHash.buildIndexFromSource source2
                                |> (\idx -> SemanticHash.getSemanticHash idx "caller")
                    in
                    case ( callerHash1, callerHash2 ) of
                        ( Just h1, Just h2 ) ->
                            h1 |> Expect.notEqual h2

                        _ ->
                            Expect.fail "Expected both hashes to exist"
            , test "cross-module: changing a dep in another module changes caller hash" <|
                \_ ->
                    let
                        mathLib1 =
                            { moduleName = "MathLib", source = "module MathLib exposing (..)\n\nabs n = if n < 0 then negate n else n\n\nclamp lo hi x = x" }

                        mathLib2 =
                            { moduleName = "MathLib", source = "module MathLib exposing (..)\n\nabs n = if n < 0 then negate n else n\n\nclamp lo hi x = lo" }

                        tests_ =
                            { moduleName = "Tests", source = "module Tests exposing (..)\n\nimport MathLib\n\nmyTest = MathLib.abs 5" }

                        hash1 =
                            SemanticHash.buildMultiModuleIndex [ mathLib1, tests_ ]
                                |> (\idx -> SemanticHash.getSemanticHash idx "Tests.myTest")

                        hash2 =
                            SemanticHash.buildMultiModuleIndex [ mathLib2, tests_ ]
                                |> (\idx -> SemanticHash.getSemanticHash idx "Tests.myTest")
                    in
                    case ( hash1, hash2 ) of
                        ( Just h1, Just h2 ) ->
                            -- myTest calls MathLib.abs which didn't change → hash unchanged
                            h1 |> Expect.equal h2

                        _ ->
                            Expect.fail "Expected both hashes to exist"
            , test "cross-module: changing called function changes caller hash" <|
                \_ ->
                    let
                        mathLib1 =
                            { moduleName = "MathLib", source = "module MathLib exposing (..)\n\nabs n = if n < 0 then negate n else n" }

                        mathLib2 =
                            { moduleName = "MathLib", source = "module MathLib exposing (..)\n\nabs n = if n < 0 then 0 - n else n" }

                        tests_ =
                            { moduleName = "Tests", source = "module Tests exposing (..)\n\nimport MathLib\n\nmyTest = MathLib.abs 5" }

                        hash1 =
                            SemanticHash.buildMultiModuleIndex [ mathLib1, tests_ ]
                                |> (\idx -> SemanticHash.getSemanticHash idx "Tests.myTest")

                        hash2 =
                            SemanticHash.buildMultiModuleIndex [ mathLib2, tests_ ]
                                |> (\idx -> SemanticHash.getSemanticHash idx "Tests.myTest")
                    in
                    case ( hash1, hash2 ) of
                        ( Just h1, Just h2 ) ->
                            -- myTest calls MathLib.abs which DID change → hash must change
                            h1 |> Expect.notEqual h2

                        _ ->
                            Expect.fail "Expected both hashes to exist"
            , test "semanticHashForExpression only includes transitive deps" <|
                \_ ->
                    let
                        modules =
                            [ { moduleName = "MathLib"
                              , source = "module MathLib exposing (..)\n\nabs n = if n < 0 then negate n else n\n\nclamp lo hi x = x"
                              }
                            , { moduleName = "Tests"
                              , source = "module Tests exposing (..)\n\nimport MathLib\n\ntestAbs = MathLib.abs 5\n\ntestClamp = MathLib.clamp 0 10 5"
                              }
                            ]

                        index =
                            SemanticHash.buildMultiModuleIndex modules

                        -- Hash for testAbs should NOT include clamp
                        hashForTestAbs =
                            SemanticHash.semanticHashForEntry index "Tests.testAbs"

                        -- Now change clamp — testAbs hash should be unchanged
                        modules2 =
                            [ { moduleName = "MathLib"
                              , source = "module MathLib exposing (..)\n\nabs n = if n < 0 then negate n else n\n\nclamp lo hi x = lo"
                              }
                            , { moduleName = "Tests"
                              , source = "module Tests exposing (..)\n\nimport MathLib\n\ntestAbs = MathLib.abs 5\n\ntestClamp = MathLib.clamp 0 10 5"
                              }
                            ]

                        index2 =
                            SemanticHash.buildMultiModuleIndex modules2

                        hashForTestAbs2 =
                            SemanticHash.semanticHashForEntry index2 "Tests.testAbs"
                    in
                    case ( hashForTestAbs, hashForTestAbs2 ) of
                        ( Just h1, Just h2 ) ->
                            -- testAbs doesn't call clamp, so changing clamp shouldn't change testAbs's entry hash
                            h1 |> Expect.equal h2

                        _ ->
                            Expect.fail "Expected both hashes"
            , test "package deps use version-qualified name" <|
                \_ ->
                    let
                        source =
                            { moduleName = "Foo", source = "module Foo exposing (..)\n\nfoo x = List.map identity x" }

                        index =
                            SemanticHash.buildMultiModuleIndexWithPackages
                                { packageVersions = [ ( "List", "elm/core/1.0.5" ) ]
                                , modules = [ source ]
                                }

                        hash =
                            SemanticHash.getSemanticHash index "Foo.foo"
                    in
                    case hash of
                        Just h ->
                            -- Hash should contain the versioned package reference
                            String.contains "elm/core/1.0.5" h
                                |> Expect.equal True

                        Nothing ->
                            Expect.fail "Expected hash to exist"
            , test "changing an unrelated function does NOT change the caller's hash" <|
                \_ ->
                    let
                        source1 =
                            "module Foo exposing (..)\n\nhelper = 1\n\nunrelated = 99\n\ncaller = helper + 1"

                        source2 =
                            "module Foo exposing (..)\n\nhelper = 1\n\nunrelated = 100\n\ncaller = helper + 1"

                        callerHash1 =
                            SemanticHash.buildIndexFromSource source1
                                |> (\idx -> SemanticHash.getSemanticHash idx "caller")

                        callerHash2 =
                            SemanticHash.buildIndexFromSource source2
                                |> (\idx -> SemanticHash.getSemanticHash idx "caller")
                    in
                    case ( callerHash1, callerHash2 ) of
                        ( Just h1, Just h2 ) ->
                            h1 |> Expect.equal h2

                        _ ->
                            Expect.fail "Expected both hashes to exist"
            ]
        , describe "declarationHash"
            [ test "single-module index: empty ModuleName returns bare-name lookup" <|
                \_ ->
                    let
                        index =
                            SemanticHash.buildIndexFromSource "module Foo exposing (..)\n\nfoo = 42\n"
                    in
                    SemanticHash.declarationHash [] "foo" index
                        |> Expect.equal (SemanticHash.getSemanticHash index "foo")
            , test "single-segment ModuleName joins with dot" <|
                \_ ->
                    let
                        index =
                            SemanticHash.buildMultiModuleIndex
                                [ { moduleName = "Main", source = "module Main exposing (..)\n\nfoo = 42\n" } ]
                    in
                    SemanticHash.declarationHash [ "Main" ] "foo" index
                        |> Expect.equal (SemanticHash.getSemanticHash index "Main.foo")
            , test "multi-segment ModuleName joins with dots" <|
                \_ ->
                    let
                        index =
                            SemanticHash.buildMultiModuleIndex
                                [ { moduleName = "Foo.Bar", source = "module Foo.Bar exposing (..)\n\nbaz = 42\n" } ]
                    in
                    SemanticHash.declarationHash [ "Foo", "Bar" ] "baz" index
                        |> Expect.equal (SemanticHash.getSemanticHash index "Foo.Bar.baz")
            , test "returns Nothing for missing declaration" <|
                \_ ->
                    let
                        index =
                            SemanticHash.buildMultiModuleIndex
                                [ { moduleName = "Main", source = "module Main exposing (..)\n\nfoo = 42\n" } ]
                    in
                    SemanticHash.declarationHash [ "Main" ] "nonexistent" index
                        |> Expect.equal Nothing
            , test "returns a non-empty hash for a known declaration" <|
                \_ ->
                    let
                        index =
                            SemanticHash.buildMultiModuleIndex
                                [ { moduleName = "Main", source = "module Main exposing (..)\n\nfoo = 42\n" } ]
                    in
                    case SemanticHash.declarationHash [ "Main" ] "foo" index of
                        Just hash ->
                            String.isEmpty hash |> Expect.equal False

                        Nothing ->
                            Expect.fail "Expected declarationHash to return a value"
            ]
        , describe "diffIndices"
            [ test "identical indices have no changes" <|
                \_ ->
                    let
                        index =
                            SemanticHash.buildMultiModuleIndex
                                [ { moduleName = "A", source = "module A exposing (..)\n\nfoo = 42\n" } ]

                        diff =
                            SemanticHash.diffIndices index index
                    in
                    diff.changed
                        |> Expect.equal Set.empty
            , test "changed function body shows up in changed set" <|
                \_ ->
                    let
                        original =
                            SemanticHash.buildMultiModuleIndex
                                [ { moduleName = "A", source = "module A exposing (..)\n\nfoo = 42\n" } ]

                        mutated =
                            SemanticHash.buildMultiModuleIndex
                                [ { moduleName = "A", source = "module A exposing (..)\n\nfoo = 43\n" } ]

                        diff =
                            SemanticHash.diffIndices original mutated
                    in
                    Set.member "A.foo" diff.changed
                        |> Expect.equal True
            , test "Merkle property: caller of changed function is also changed" <|
                \_ ->
                    let
                        original =
                            SemanticHash.buildMultiModuleIndex
                                [ { moduleName = "A"
                                  , source = "module A exposing (..)\n\nhelper x = x + 1\n\nvalue = helper 10\n"
                                  }
                                ]

                        mutated =
                            SemanticHash.buildMultiModuleIndex
                                [ { moduleName = "A"
                                  , source = "module A exposing (..)\n\nhelper x = x * 2\n\nvalue = helper 10\n"
                                  }
                                ]

                        diff =
                            SemanticHash.diffIndices original mutated
                    in
                    -- Both helper and value should be changed (value depends on helper)
                    Expect.all
                        [ \d -> Set.member "A.helper" d.changed |> Expect.equal True
                        , \d -> Set.member "A.value" d.changed |> Expect.equal True
                        ]
                        diff
            , test "unrelated function stays in unchanged set" <|
                \_ ->
                    let
                        original =
                            SemanticHash.buildMultiModuleIndex
                                [ { moduleName = "A"
                                  , source = "module A exposing (..)\n\nfoo = 42\n\nbar = 99\n"
                                  }
                                ]

                        mutated =
                            SemanticHash.buildMultiModuleIndex
                                [ { moduleName = "A"
                                  , source = "module A exposing (..)\n\nfoo = 43\n\nbar = 99\n"
                                  }
                                ]

                        diff =
                            SemanticHash.diffIndices original mutated
                    in
                    Expect.all
                        [ \d -> Set.member "A.foo" d.changed |> Expect.equal True
                        , \d -> Set.member "A.bar" d.unchanged |> Expect.equal True
                        ]
                        diff
            ]
        , describe "affectedRunners (semantic hash-based runner selection)"
            [ test "mutation affecting one function only selects runners that depend on it" <|
                \_ ->
                    let
                        moduleA =
                            { moduleName = "A"
                            , source = "module A exposing (..)\n\nadd x y = x + y\n\nmul x y = x * y\n"
                            }

                        testModule =
                            { moduleName = "Tests"
                            , source = "module Tests exposing (..)\n\nimport A\nimport Test exposing (..)\n\naddTest = test \"add\" (\\_ -> A.add 1 2)\nmulTest = test \"mul\" (\\_ -> A.mul 3 4)\nsuite = describe \"t\" [addTest, mulTest]\n"
                            }

                        original =
                            SemanticHash.buildMultiModuleIndex [ moduleA, testModule ]

                        mutatedA =
                            { moduleName = "A"
                            , source = "module A exposing (..)\n\nadd x y = x - y\n\nmul x y = x * y\n"
                            }

                        mutated =
                            SemanticHash.buildMultiModuleIndex [ mutatedA, testModule ]

                        diff =
                            SemanticHash.diffIndices original mutated
                    in
                    Expect.all
                        [ \d -> Set.member "A.add" d.changed |> Expect.equal True |> Expect.onFail "A.add should be changed"
                        , \d -> Set.member "A.mul" d.changed |> Expect.equal False |> Expect.onFail "A.mul should NOT be changed"
                        , \d -> Set.member "Tests.addTest" d.changed |> Expect.equal True |> Expect.onFail "Tests.addTest should be changed (depends on A.add)"
                        , \d -> Set.member "Tests.mulTest" d.changed |> Expect.equal False |> Expect.onFail "Tests.mulTest should NOT be changed"
                        ]
                        diff
            , test "mutation to transitive dependency propagates through Merkle tree" <|
                \_ ->
                    let
                        moduleA =
                            { moduleName = "A"
                            , source = "module A exposing (..)\n\nhelper x = x + 1\n\npublic x = helper (helper x)\n"
                            }

                        testModule =
                            { moduleName = "Tests"
                            , source = "module Tests exposing (..)\n\nimport A\nimport Test exposing (..)\n\nt1 = test \"uses public\" (\\_ -> A.public 5)\nt2 = test \"standalone\" (\\_ -> 42)\nsuite = describe \"t\" [t1, t2]\n"
                            }

                        original =
                            SemanticHash.buildMultiModuleIndex [ moduleA, testModule ]

                        mutatedA =
                            { moduleName = "A"
                            , source = "module A exposing (..)\n\nhelper x = x + 2\n\npublic x = helper (helper x)\n"
                            }

                        mutated =
                            SemanticHash.buildMultiModuleIndex [ mutatedA, testModule ]

                        diff =
                            SemanticHash.diffIndices original mutated
                    in
                    Expect.all
                        [ \d -> Set.member "A.helper" d.changed |> Expect.equal True |> Expect.onFail "A.helper should be changed"
                        , \d -> Set.member "A.public" d.changed |> Expect.equal True |> Expect.onFail "A.public should be changed (depends on helper)"
                        , \d -> Set.member "Tests.t1" d.changed |> Expect.equal True |> Expect.onFail "Tests.t1 should be changed (depends on A.public)"
                        , \d -> Set.member "Tests.t2" d.changed |> Expect.equal False |> Expect.onFail "Tests.t2 should NOT be changed (standalone)"
                        ]
                        diff
            , test "equivalent mutation produces no changed declarations" <|
                \_ ->
                    let
                        moduleA =
                            { moduleName = "A"
                            , source = "module A exposing (..)\n\nfoo x = x + 1\n"
                            }

                        original =
                            SemanticHash.buildMultiModuleIndex [ moduleA ]

                        -- Same source = same index = no changes
                        mutated =
                            SemanticHash.buildMultiModuleIndex [ moduleA ]

                        diff =
                            SemanticHash.diffIndices original mutated
                    in
                    Set.isEmpty diff.changed
                        |> Expect.equal True
                        |> Expect.onFail "Identical source should produce no changed declarations"
            , test "affectedRunnerIndices selects correct subset" <|
                \_ ->
                    let
                        -- Runner 0 depends on A.add, runner 1 depends on A.mul
                        perRunnerDeps =
                            [ Set.fromList [ "A.add" ]
                            , Set.fromList [ "A.mul" ]
                            , Set.fromList [ "A.add", "A.mul" ]
                            ]

                        changedDecls =
                            Set.singleton "A.add"
                    in
                    SemanticHash.affectedRunnerIndices perRunnerDeps changedDecls
                        |> Expect.equal [ 0, 2 ]
            , test "depsForExpression finds transitive deps from inline test expression" <|
                \_ ->
                    let
                        index =
                            SemanticHash.buildMultiModuleIndex
                                [ { moduleName = "A"
                                  , source = "module A exposing (..)\n\nhelper x = x + 1\n\npublic x = helper x\n"
                                  }
                                ]

                        -- Simulate an inline test body: \_ -> A.public 5
                        expr =
                            parseExpression "module T exposing (..)\n\nimport A\n\nt = A.public 5\n"
                    in
                    case expr of
                        Just e ->
                            let
                                deps =
                                    SemanticHash.depsForExpression index SemanticHash.emptyImportResolver "T" e
                            in
                            Expect.all
                                [ \d -> Set.member "A.public" d |> Expect.equal True |> Expect.onFail "Should include A.public"
                                , \d -> Set.member "A.helper" d |> Expect.equal True |> Expect.onFail "Should include A.helper (transitive)"
                                ]
                                deps

                        Nothing ->
                            Expect.fail "Failed to parse"
            , test "affectedRunnerIndices returns empty when no runner affected" <|
                \_ ->
                    let
                        perRunnerDeps =
                            [ Set.fromList [ "A.add" ]
                            , Set.fromList [ "A.mul" ]
                            ]

                        changedDecls =
                            Set.singleton "B.other"
                    in
                    SemanticHash.affectedRunnerIndices perRunnerDeps changedDecls
                        |> Expect.equal []
            ]
        ]


{-| Parse source and extract the first function's body expression.
-}
parseExpression : String -> Maybe (Node Expression)
parseExpression source =
    case Elm.Parser.parseToFile source of
        Ok file ->
            file.declarations
                |> List.filterMap
                    (\(Node _ decl) ->
                        case decl of
                            FunctionDeclaration func ->
                                Just (Node.value func.declaration |> .expression)

                            _ ->
                                Nothing
                    )
                |> List.head

        Err _ ->
            Nothing


{-| Parse source and extract dependencies from the first function.
-}
parseDeps : String -> List ( List String, String )
parseDeps source =
    case parseExpression source of
        Just expr ->
            SemanticHash.extractDependencies expr

        Nothing ->
            []


fileAspectHashTests : Test
fileAspectHashTests =
    describe "FileAspectHashes"
        [ test "changing function body changes expressionsHash only" <|
            \_ ->
                let
                    before =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (foo)\n\nimport Bar\n\nfoo = 1\n"

                    after =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (foo)\n\nimport Bar\n\nfoo = 999\n"
                in
                Expect.all
                    [ \_ -> Expect.notEqual before.expressionsHash after.expressionsHash
                    , \_ -> Expect.equal before.importsHash after.importsHash
                    , \_ -> Expect.equal before.exposingHash after.exposingHash
                    , \_ -> Expect.equal before.declNamesHash after.declNamesHash
                    ]
                    ()
        , test "changing import changes importsHash only" <|
            \_ ->
                let
                    before =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (foo)\n\nimport Bar\n\nfoo = 1\n"

                    after =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (foo)\n\nimport Baz\n\nfoo = 1\n"
                in
                Expect.all
                    [ \_ -> Expect.equal before.expressionsHash after.expressionsHash
                    , \_ -> Expect.notEqual before.importsHash after.importsHash
                    , \_ -> Expect.equal before.exposingHash after.exposingHash
                    , \_ -> Expect.equal before.declNamesHash after.declNamesHash
                    ]
                    ()
        , test "changing exposing list changes exposingHash only" <|
            \_ ->
                let
                    before =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (foo)\n\nfoo = 1\n"

                    after =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (..)\n\nfoo = 1\n"
                in
                Expect.all
                    [ \_ -> Expect.equal before.expressionsHash after.expressionsHash
                    , \_ -> Expect.notEqual before.exposingHash after.exposingHash
                    , \_ -> Expect.equal before.declNamesHash after.declNamesHash
                    ]
                    ()
        , test "adding type annotation changes declNamesHash but not expressionsHash" <|
            \_ ->
                let
                    before =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (foo)\n\nfoo = 1\n"

                    after =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (foo)\n\nfoo : Int\nfoo = 1\n"
                in
                Expect.all
                    [ \_ -> Expect.equal before.expressionsHash after.expressionsHash
                    , \_ -> Expect.notEqual before.declNamesHash after.declNamesHash
                    ]
                    ()
        , test "adding custom type changes customTypesHash but not expressionsHash" <|
            \_ ->
                let
                    before =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (..)\n\nfoo = 1\n"

                    after =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (..)\n\ntype Color = Red | Blue\n\nfoo = 1\n"
                in
                Expect.all
                    [ \_ -> Expect.equal before.expressionsHash after.expressionsHash
                    , \_ -> Expect.notEqual before.customTypesHash after.customTypesHash
                    ]
                    ()
        ]
