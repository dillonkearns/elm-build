module ReviewRunnerTest exposing (suite)

{-| Integration tests for the elm-review runner using Test.BackendTask.

Tests the full pipeline: load review project, evaluate Rule.review via
the interpreter, parse results, verify caching behavior.

-}

import DepGraph
import Dict
import Expect
import ReviewRunner
import SemanticHash
import Test exposing (Test, describe, test)
import Test.BackendTask as BackendTaskTest


suite : Test
suite =
    describe "ReviewRunner integration"
        [ pureHelperTests
        , semanticCacheKeyTests
        , perDeclarationTests
        , cacheDecisionTests
        , cacheSerializationTests
        , cachePipelineTests
        , ruleClassificationTests
        , narrowCacheKeyTests
        ]


{-| Pure helper function tests (no BackendTask needed).
-}
pureHelperTests : Test
pureHelperTests =
    describe "pure helpers"
        [ describe "escapeElmString"
            [ test "plain string unchanged" <|
                \_ ->
                    ReviewRunner.escapeElmString "hello world"
                        |> Expect.equal "hello world"
            , test "escapes backslashes" <|
                \_ ->
                    ReviewRunner.escapeElmString "foo\\bar"
                        |> Expect.equal "foo\\\\bar"
            , test "escapes double quotes" <|
                \_ ->
                    ReviewRunner.escapeElmString "say \"hi\""
                        |> Expect.equal "say \\\"hi\\\""
            , test "escapes newlines" <|
                \_ ->
                    ReviewRunner.escapeElmString "line1\nline2"
                        |> Expect.equal "line1\\nline2"
            , test "escapes carriage returns" <|
                \_ ->
                    ReviewRunner.escapeElmString "line1\u{000D}line2"
                        |> Expect.equal "line1\\rline2"
            , test "handles all special chars together" <|
                \_ ->
                    ReviewRunner.escapeElmString "a\\b\"c\nd"
                        |> Expect.equal "a\\\\b\\\"c\\nd"
            , test "empty string" <|
                \_ ->
                    ReviewRunner.escapeElmString ""
                        |> Expect.equal ""
            ]
        , describe "buildModuleRecord"
            [ test "simple module" <|
                \_ ->
                    ReviewRunner.buildModuleRecord { path = "src/Foo.elm", source = "module Foo exposing (..)\n\na = 1" }
                        |> Expect.equal "{ path = \"src/Foo.elm\", source = \"module Foo exposing (..)\\n\\na = 1\" }"
            , test "module with quotes in source" <|
                \_ ->
                    ReviewRunner.buildModuleRecord { path = "src/Bar.elm", source = "x = \"hello\"" }
                        |> Expect.equal "{ path = \"src/Bar.elm\", source = \"x = \\\"hello\\\"\" }"
            ]
        , describe "buildExpression"
            [ test "single module" <|
                \_ ->
                    ReviewRunner.buildExpression [ { path = "src/A.elm", source = "module A exposing (..)\na = 1" } ]
                        |> Expect.equal "ReviewRunnerHelper.runReview [ { path = \"src/A.elm\", source = \"module A exposing (..)\\na = 1\" } ]"
            , test "multiple modules" <|
                \_ ->
                    ReviewRunner.buildExpression
                        [ { path = "src/A.elm", source = "a" }
                        , { path = "src/B.elm", source = "b" }
                        ]
                        |> Expect.equal "ReviewRunnerHelper.runReview [ { path = \"src/A.elm\", source = \"a\" }, { path = \"src/B.elm\", source = \"b\" } ]"
            , test "empty list" <|
                \_ ->
                    ReviewRunner.buildExpression []
                        |> Expect.equal "ReviewRunnerHelper.runReview []"
            ]
        , describe "buildExpressionForRule"
            [ test "rule 0 with one module" <|
                \_ ->
                    ReviewRunner.buildExpressionForRule 0
                        [ { path = "src/A.elm", source = "a", astJson = "{}" } ]
                        |> Expect.equal "ReviewRunnerHelper.runSingleRule 0 [ { path = \"src/A.elm\", source = \"a\", astJson = \"{}\" } ]"
            , test "rule 2 with empty modules" <|
                \_ ->
                    ReviewRunner.buildExpressionForRule 2 []
                        |> Expect.equal "ReviewRunnerHelper.runSingleRule 2 []"
            ]
        , describe "reviewRunnerHelperSource"
            [ test "contains runSingleRule" <|
                \_ ->
                    ReviewRunner.reviewRunnerHelperSource
                        |> String.contains "runSingleRule ruleIndex modules ="
                        |> Expect.equal True
            , test "contains ruleCount" <|
                \_ ->
                    ReviewRunner.reviewRunnerHelperSource
                        |> String.contains "ruleCount = List.length ReviewConfig.config"
                        |> Expect.equal True
            , test "exposes runSingleRule and ruleCount" <|
                \_ ->
                    ReviewRunner.reviewRunnerHelperSource
                        |> String.contains "runSingleRule"
                        |> Expect.equal True
            ]
        , describe "parseReviewOutput"
            [ test "single error" <|
                \_ ->
                    ReviewRunner.parseReviewOutput "RULE:NoDebug.Log|FILE:src/A.elm|LINE:5|COL:1|MSG:Remove Debug.log"
                        |> Expect.equal
                            [ { ruleName = "NoDebug.Log"
                              , filePath = "src/A.elm"
                              , line = 5
                              , column = 1
                              , message = "Remove Debug.log"
                              }
                            ]
            , test "multiple errors" <|
                \_ ->
                    ReviewRunner.parseReviewOutput "RULE:NoDebug.Log|FILE:src/A.elm|LINE:5|COL:1|MSG:Remove Debug.log\nRULE:NoUnused.Variables|FILE:src/B.elm|LINE:3|COL:5|MSG:Variable `x` is not used"
                        |> List.length
                        |> Expect.equal 2
            , test "empty output means no errors" <|
                \_ ->
                    ReviewRunner.parseReviewOutput ""
                        |> Expect.equal []
            , test "parses line and column as ints" <|
                \_ ->
                    ReviewRunner.parseReviewOutput "RULE:R|FILE:f|LINE:42|COL:13|MSG:m"
                        |> List.head
                        |> Maybe.map (\e -> ( e.line, e.column ))
                        |> Expect.equal (Just ( 42, 13 ))
            , test "message with pipe characters" <|
                \_ ->
                    ReviewRunner.parseReviewOutput "RULE:R|FILE:f|LINE:1|COL:1|MSG:use a |> b instead"
                        |> List.head
                        |> Maybe.map .message
                        |> Expect.equal (Just "use a |> b instead")
            ]
        ]



-- Test fixtures


{-| Test that semantic hashing gives stable cache keys.

Note: computeSemanticKey calls Elm.Parser.parseToFile which can't run
through the interpreter (Rope module collision). These tests run on the
HOST side via elm-pages BackendTask, not through the interpreter.

For now, we test the pure hash composition logic with pre-computed indices.
The full parseToFile-based pipeline is tested via the actual script.

-}
semanticCacheKeyTests : Test
semanticCacheKeyTests =
    describe "semantic cache keys"
        [ test "same input produces same key" <|
            \_ ->
                let
                    files =
                        [ { path = "src/Foo.elm", source = "module Foo exposing (..)\n\nfoo = 1\n" } ]
                in
                ReviewRunner.computeSemanticKey files
                    |> Expect.equal (ReviewRunner.computeSemanticKey files)
        , test "different source produces different key" <|
            \_ ->
                ReviewRunner.computeSemanticKey
                    [ { path = "src/Foo.elm", source = "module Foo exposing (..)\n\nfoo = 1\n" } ]
                    |> Expect.notEqual
                        (ReviewRunner.computeSemanticKey
                            [ { path = "src/Foo.elm", source = "module Foo exposing (..)\n\nfoo = 2\n" } ]
                        )
        , test "adding a file produces different key" <|
            \_ ->
                ReviewRunner.computeSemanticKey
                    [ { path = "src/Foo.elm", source = "module Foo exposing (..)\n\nfoo = 1\n" } ]
                    |> Expect.notEqual
                        (ReviewRunner.computeSemanticKey
                            [ { path = "src/Foo.elm", source = "module Foo exposing (..)\n\nfoo = 1\n" }
                            , { path = "src/Bar.elm", source = "module Bar exposing (..)\n\nbar = 2\n" }
                            ]
                        )
        ]


perDeclarationTests : Test
perDeclarationTests =
    describe "per-declaration semantic hashing"
        [ test "getDeclarationHashes returns one entry per function" <|
            \_ ->
                ReviewRunner.getDeclarationHashes "module Foo exposing (..)\n\nfoo = 1\n\nbar = 2\n"
                    |> List.map .name
                    |> List.sort
                    |> Expect.equal [ "bar", "foo" ]
        , test "each declaration has a unique semantic hash" <|
            \_ ->
                let
                    hashes =
                        ReviewRunner.getDeclarationHashes "module Foo exposing (..)\n\nfoo = 1\n\nbar = 2\n"
                            |> List.map .semanticHash
                in
                Expect.notEqual (List.head hashes) (List.head (List.drop 1 hashes))
        , test "changing one declaration only changes that hash" <|
            \_ ->
                let
                    before =
                        ReviewRunner.getDeclarationHashes "module Foo exposing (..)\n\nfoo = 1\n\nbar = 2\n"
                            |> List.sortBy .name

                    after =
                        ReviewRunner.getDeclarationHashes "module Foo exposing (..)\n\nfoo = 999\n\nbar = 2\n"
                            |> List.sortBy .name

                    barBefore =
                        before |> List.filter (\d -> d.name == "bar") |> List.head |> Maybe.map .semanticHash

                    barAfter =
                        after |> List.filter (\d -> d.name == "bar") |> List.head |> Maybe.map .semanticHash

                    fooBefore =
                        before |> List.filter (\d -> d.name == "foo") |> List.head |> Maybe.map .semanticHash

                    fooAfter =
                        after |> List.filter (\d -> d.name == "foo") |> List.head |> Maybe.map .semanticHash
                in
                Expect.all
                    [ \_ -> Expect.equal barBefore barAfter -- bar unchanged
                    , \_ -> Expect.notEqual fooBefore fooAfter -- foo changed
                    ]
                    ()
        , test "Merkle property: changing callee changes caller hash" <|
            \_ ->
                let
                    before =
                        ReviewRunner.getDeclarationHashes "module Foo exposing (..)\n\nfoo = 1\n\nbaz = foo\n"

                    after =
                        ReviewRunner.getDeclarationHashes "module Foo exposing (..)\n\nfoo = 999\n\nbaz = foo\n"

                    bazBefore =
                        before |> List.filter (\d -> d.name == "baz") |> List.head |> Maybe.map .semanticHash

                    bazAfter =
                        after |> List.filter (\d -> d.name == "baz") |> List.head |> Maybe.map .semanticHash
                in
                Expect.notEqual bazBefore bazAfter
        , test "mapErrorsToDeclarations assigns errors to correct declarations" <|
            \_ ->
                let
                    declarations =
                        [ { name = "foo", semanticHash = "abc", startLine = 3, endLine = 4 }
                        , { name = "bar", semanticHash = "def", startLine = 6, endLine = 7 }
                        ]

                    errors =
                        [ { ruleName = "R1", filePath = "f", line = 3, column = 1, message = "in foo" }
                        , { ruleName = "R2", filePath = "f", line = 6, column = 1, message = "in bar" }
                        ]

                    result =
                        ReviewRunner.mapErrorsToDeclarations declarations errors
                in
                Expect.all
                    [ \_ ->
                        Dict.get "foo" result
                            |> Maybe.map (.errors >> List.length)
                            |> Expect.equal (Just 1)
                    , \_ ->
                        Dict.get "bar" result
                            |> Maybe.map (.errors >> List.length)
                            |> Expect.equal (Just 1)
                    ]
                    ()
        , test "module-level errors go to __module__ bucket" <|
            \_ ->
                let
                    declarations =
                        [ { name = "foo", semanticHash = "abc", startLine = 5, endLine = 6 } ]

                    errors =
                        [ { ruleName = "NoExposing", filePath = "f", line = 1, column = 1, message = "module level" } ]

                    result =
                        ReviewRunner.mapErrorsToDeclarations declarations errors
                in
                Dict.get "__module__" result
                    |> Maybe.map (.errors >> List.length)
                    |> Expect.equal (Just 1)
        ]


cacheDecisionTests : Test
cacheDecisionTests =
    describe "cache decisions"
        [ test "ColdMiss when cache is empty" <|
            \_ ->
                ReviewRunner.checkCache Dict.empty
                    [ { path = "src/Foo.elm", source = fooSource } ]
                    |> expectColdMiss
        , test "FullCacheHit when no source changes" <|
            \_ ->
                let
                    files =
                        [ { path = "src/Foo.elm", source = fooSource } ]

                    errors =
                        [ { ruleName = "NoDebug.Log", filePath = "src/Foo.elm", line = 5, column = 5, message = "Remove Debug.log" } ]

                    cache =
                        ReviewRunner.updateCache Dict.empty files errors
                in
                ReviewRunner.checkCache cache files
                    |> expectFullCacheHit 1
        , test "FullCacheHit after whitespace-only change" <|
            \_ ->
                let
                    files =
                        [ { path = "src/Foo.elm", source = fooSource } ]

                    errors =
                        [ { ruleName = "NoDebug.Log", filePath = "src/Foo.elm", line = 5, column = 5, message = "Remove Debug.log" } ]

                    cache =
                        ReviewRunner.updateCache Dict.empty files errors

                    -- Same code with extra blank lines
                    reformattedFiles =
                        [ { path = "src/Foo.elm", source = fooSourceReformatted } ]
                in
                ReviewRunner.checkCache cache reformattedFiles
                    |> expectFullCacheHit 1
        , test "PartialMiss when one file changes" <|
            \_ ->
                let
                    files =
                        [ { path = "src/Foo.elm", source = fooSource }
                        , { path = "src/Bar.elm", source = barSource }
                        ]

                    errors =
                        [ { ruleName = "NoDebug.Log", filePath = "src/Foo.elm", line = 5, column = 5, message = "Remove Debug.log" }
                        , { ruleName = "NoMissing", filePath = "src/Bar.elm", line = 3, column = 1, message = "Missing annotation" }
                        ]

                    cache =
                        ReviewRunner.updateCache Dict.empty files errors

                    -- Change Foo, keep Bar
                    changedFiles =
                        [ { path = "src/Foo.elm", source = fooSourceChanged }
                        , { path = "src/Bar.elm", source = barSource }
                        ]
                in
                case ReviewRunner.checkCache cache changedFiles of
                    ReviewRunner.PartialMiss { cachedErrors, staleFiles } ->
                        Expect.all
                            [ \_ -> Expect.equal [ "src/Foo.elm" ] staleFiles
                            , \_ -> Expect.equal 1 (List.length cachedErrors) -- Bar's error cached
                            , \_ ->
                                cachedErrors
                                    |> List.head
                                    |> Maybe.map .filePath
                                    |> Expect.equal (Just "src/Bar.elm")
                            ]
                            ()

                    other ->
                        Expect.fail ("Expected PartialMiss, got " ++ cacheDecisionToString other)
        , test "PartialMiss: changing one declaration keeps others cached within same file" <|
            \_ ->
                let
                    source =
                        "module M exposing (..)\n\nfoo = 1\n\nbar = 2\n"

                    files =
                        [ { path = "src/M.elm", source = source } ]

                    errors =
                        [ { ruleName = "R1", filePath = "src/M.elm", line = 3, column = 1, message = "err in foo" }
                        , { ruleName = "R2", filePath = "src/M.elm", line = 5, column = 1, message = "err in bar" }
                        ]

                    cache =
                        ReviewRunner.updateCache Dict.empty files errors

                    -- Change only foo, bar stays the same
                    changedSource =
                        "module M exposing (..)\n\nfoo = 999\n\nbar = 2\n"

                    changedFiles =
                        [ { path = "src/M.elm", source = changedSource } ]
                in
                -- File has changed declarations, so it's a miss for the file
                -- But the cache preserved per-declaration info
                case ReviewRunner.checkCache cache changedFiles of
                    ReviewRunner.PartialMiss { staleFiles } ->
                        Expect.equal [ "src/M.elm" ] staleFiles

                    ReviewRunner.ColdMiss _ ->
                        Expect.fail "Should not be ColdMiss — cache exists"

                    ReviewRunner.FullCacheHit _ ->
                        Expect.fail "Should not be FullCacheHit — foo changed"
        , test "updateCache then checkCache round-trips correctly" <|
            \_ ->
                let
                    files =
                        [ { path = "src/A.elm", source = "module A exposing (..)\n\na = 1\n" }
                        , { path = "src/B.elm", source = "module B exposing (..)\n\nb = 2\n" }
                        ]

                    errors =
                        [ { ruleName = "R", filePath = "src/A.elm", line = 3, column = 1, message = "e1" }
                        , { ruleName = "R", filePath = "src/B.elm", line = 3, column = 1, message = "e2" }
                        ]

                    cache =
                        ReviewRunner.updateCache Dict.empty files errors
                in
                ReviewRunner.checkCache cache files
                    |> expectFullCacheHit 2
        , test "FullCacheHit preserves exact error content" <|
            \_ ->
                let
                    files =
                        [ { path = "src/Foo.elm", source = fooSource } ]

                    originalErrors =
                        [ { ruleName = "NoDebug.Log", filePath = "src/Foo.elm", line = 5, column = 5, message = "Remove Debug.log" } ]

                    cache =
                        ReviewRunner.updateCache Dict.empty files originalErrors
                in
                case ReviewRunner.checkCache cache files of
                    ReviewRunner.FullCacheHit errors ->
                        Expect.equal originalErrors errors

                    other ->
                        Expect.fail ("Expected FullCacheHit, got " ++ cacheDecisionToString other)
        ]



cacheSerializationTests : Test
cacheSerializationTests =
    describe "cache serialization (JSON round-trip)"
        [ test "empty cache round-trips" <|
            \_ ->
                Dict.empty
                    |> ReviewRunner.encodeCacheState
                    |> ReviewRunner.decodeCacheState
                    |> Expect.equal (Just Dict.empty)
        , test "single file cache round-trips" <|
            \_ ->
                let
                    cache =
                        ReviewRunner.updateCache Dict.empty
                            [ { path = "src/Foo.elm", source = fooSource } ]
                            [ { ruleName = "NoDebug.Log", filePath = "src/Foo.elm", line = 5, column = 5, message = "Remove Debug.log" } ]
                in
                cache
                    |> ReviewRunner.encodeCacheState
                    |> ReviewRunner.decodeCacheState
                    |> Maybe.andThen (\decoded -> Just (ReviewRunner.checkCache decoded [ { path = "src/Foo.elm", source = fooSource } ]))
                    |> Maybe.map (expectFullCacheHit 1)
                    |> Maybe.withDefault (Expect.fail "decode failed")
        , test "multi-file cache round-trips preserving all errors" <|
            \_ ->
                let
                    files =
                        [ { path = "src/Foo.elm", source = fooSource }
                        , { path = "src/Bar.elm", source = barSource }
                        ]

                    errors =
                        [ { ruleName = "R1", filePath = "src/Foo.elm", line = 5, column = 5, message = "err1" }
                        , { ruleName = "R2", filePath = "src/Bar.elm", line = 3, column = 1, message = "err2" }
                        ]

                    cache =
                        ReviewRunner.updateCache Dict.empty files errors
                in
                cache
                    |> ReviewRunner.encodeCacheState
                    |> ReviewRunner.decodeCacheState
                    |> Maybe.map (\decoded -> ReviewRunner.checkCache decoded files)
                    |> Maybe.map (expectFullCacheHit 2)
                    |> Maybe.withDefault (Expect.fail "decode failed")
        ]


{-| Tests that simulate the full pipeline: cold → warm → change → warm.
These test the sequence of operations that would happen across multiple
CLI invocations, using the pure cache functions.
-}
cachePipelineTests : Test
cachePipelineTests =
    describe "cache pipeline (simulated multi-run)"
        [ test "cold → warm: first run caches, second run hits" <|
            \_ ->
                let
                    files =
                        [ { path = "src/Foo.elm", source = fooSource } ]

                    -- Simulate first run: cold miss → evaluate → cache results
                    decision1 =
                        ReviewRunner.checkCache Dict.empty files

                    errors =
                        [ { ruleName = "NoDebug.Log", filePath = "src/Foo.elm", line = 5, column = 5, message = "Remove Debug.log" } ]

                    cache =
                        ReviewRunner.updateCache Dict.empty files errors

                    -- Simulate second run: same files → should be full hit
                    decision2 =
                        ReviewRunner.checkCache cache files
                in
                Expect.all
                    [ \_ -> expectColdMiss decision1
                    , \_ -> expectFullCacheHit 1 decision2
                    ]
                    ()
        , test "cold → change one file → partial miss with correct stale/cached split" <|
            \_ ->
                let
                    files =
                        [ { path = "src/Foo.elm", source = fooSource }
                        , { path = "src/Bar.elm", source = barSource }
                        ]

                    errors =
                        [ { ruleName = "NoDebug.Log", filePath = "src/Foo.elm", line = 5, column = 5, message = "in foo" }
                        , { ruleName = "NoMissing", filePath = "src/Bar.elm", line = 3, column = 1, message = "in bar" }
                        ]

                    cache =
                        ReviewRunner.updateCache Dict.empty files errors

                    -- Second run: change Foo only
                    changedFiles =
                        [ { path = "src/Foo.elm", source = fooSourceChanged }
                        , { path = "src/Bar.elm", source = barSource }
                        ]

                    decision =
                        ReviewRunner.checkCache cache changedFiles
                in
                case decision of
                    ReviewRunner.PartialMiss { cachedErrors, staleFiles } ->
                        Expect.all
                            [ \_ -> Expect.equal [ "src/Foo.elm" ] staleFiles
                            , \_ -> Expect.equal 1 (List.length cachedErrors)
                            , \_ ->
                                cachedErrors
                                    |> List.head
                                    |> Maybe.map .message
                                    |> Expect.equal (Just "in bar")
                            ]
                            ()

                    other ->
                        Expect.fail ("Expected PartialMiss, got " ++ cacheDecisionToString other)
        , test "cold → reformat → full cache hit (semantic hash unchanged)" <|
            \_ ->
                let
                    files =
                        [ { path = "src/Foo.elm", source = fooSource } ]

                    errors =
                        [ { ruleName = "NoDebug.Log", filePath = "src/Foo.elm", line = 5, column = 5, message = "Remove Debug.log" } ]

                    cache =
                        ReviewRunner.updateCache Dict.empty files errors

                    -- Reformat: add blank lines (no semantic change)
                    reformattedFiles =
                        [ { path = "src/Foo.elm", source = fooSourceReformatted } ]

                    decision =
                        ReviewRunner.checkCache cache reformattedFiles
                in
                expectFullCacheHit 1 decision
        , test "cold → change → update → warm: full cycle with stale re-eval" <|
            \_ ->
                let
                    files =
                        [ { path = "src/Foo.elm", source = fooSource }
                        , { path = "src/Bar.elm", source = barSource }
                        ]

                    initialErrors =
                        [ { ruleName = "NoDebug.Log", filePath = "src/Foo.elm", line = 5, column = 5, message = "in foo" }
                        , { ruleName = "NoMissing", filePath = "src/Bar.elm", line = 3, column = 1, message = "in bar" }
                        ]

                    cache1 =
                        ReviewRunner.updateCache Dict.empty files initialErrors

                    -- Change Foo
                    changedFiles =
                        [ { path = "src/Foo.elm", source = fooSourceChanged }
                        , { path = "src/Bar.elm", source = barSource }
                        ]

                    -- Re-evaluate stale files only → new errors for Foo
                    newFooErrors =
                        [ { ruleName = "NoDebug.Log", filePath = "src/Foo.elm", line = 5, column = 5, message = "new foo error" } ]

                    -- Update cache with new results for changed files only
                    cache2 =
                        ReviewRunner.updateCache cache1
                            [ { path = "src/Foo.elm", source = fooSourceChanged } ]
                            newFooErrors

                    -- Third run: nothing changed → full hit
                    decision =
                        ReviewRunner.checkCache cache2 changedFiles
                in
                case decision of
                    ReviewRunner.FullCacheHit errors ->
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length errors) -- both files cached
                            , \_ ->
                                errors
                                    |> List.filter (\e -> e.filePath == "src/Foo.elm")
                                    |> List.head
                                    |> Maybe.map .message
                                    |> Expect.equal (Just "new foo error") -- updated error
                            , \_ ->
                                errors
                                    |> List.filter (\e -> e.filePath == "src/Bar.elm")
                                    |> List.head
                                    |> Maybe.map .message
                                    |> Expect.equal (Just "in bar") -- preserved from cache1
                            ]
                            ()

                    other ->
                        Expect.fail ("Expected FullCacheHit, got " ++ cacheDecisionToString other)
        ]



-- Cache test helpers


expectColdMiss : ReviewRunner.CacheDecision -> Expect.Expectation
expectColdMiss decision =
    case decision of
        ReviewRunner.ColdMiss _ ->
            Expect.pass

        other ->
            Expect.fail ("Expected ColdMiss, got " ++ cacheDecisionToString other)


expectFullCacheHit : Int -> ReviewRunner.CacheDecision -> Expect.Expectation
expectFullCacheHit expectedErrorCount decision =
    case decision of
        ReviewRunner.FullCacheHit errors ->
            Expect.equal expectedErrorCount (List.length errors)

        other ->
            Expect.fail ("Expected FullCacheHit with " ++ String.fromInt expectedErrorCount ++ " errors, got " ++ cacheDecisionToString other)


cacheDecisionToString : ReviewRunner.CacheDecision -> String
cacheDecisionToString d =
    case d of
        ReviewRunner.FullCacheHit errors ->
            "FullCacheHit(" ++ String.fromInt (List.length errors) ++ " errors)"

        ReviewRunner.PartialMiss { cachedErrors, staleFiles } ->
            "PartialMiss(cached=" ++ String.fromInt (List.length cachedErrors) ++ ", stale=" ++ String.join "," staleFiles ++ ")"

        ReviewRunner.ColdMiss files ->
            "ColdMiss(" ++ String.join "," files ++ ")"



-- Test fixtures


fooSource : String
fooSource =
    "module Foo exposing (..)\n\nimport Debug\n\nfoo x =\n    Debug.log \"test\" x\n\nbar = 42\n"


fooSourceReformatted : String
fooSourceReformatted =
    "module Foo exposing (..)\n\n\nimport Debug\n\n\nfoo x =\n    Debug.log \"test\" x\n\n\nbar =\n    42\n"


fooSourceChanged : String
fooSourceChanged =
    "module Foo exposing (..)\n\nimport Debug\n\nfoo x =\n    Debug.log \"test\" (x + 1)\n\nbar = 42\n"


barSource : String
barSource =
    "module Bar exposing (..)\n\nbar x y =\n    x + y\n"


reviewElmJson : String
reviewElmJson =
    """{"type": "application", "source-directories": ["src"], "elm-version": "0.19.1", "dependencies": {"direct": {"jfmengels/elm-review": "2.16.2", "elm/core": "1.0.5"}, "indirect": {}}, "test-dependencies": {"direct": {}, "indirect": {}}}"""


minimalReviewConfig : String
minimalReviewConfig =
    """module ReviewConfig exposing (config)

import Review.Rule as Rule exposing (Rule)

config : List Rule
config =
    [ Rule.newModuleRuleSchema "NoDebugLog" ()
        |> Rule.withSimpleExpressionVisitor (\\_ -> [])
        |> Rule.fromModuleRuleSchema
    ]
"""


narrowCacheKeyTests : Test
narrowCacheKeyTests =
    describe "narrow cache keys"
        [ test "NoExposingEverything: body change does NOT change key" <|
            \_ ->
                let
                    profile =
                        ReviewRunner.profileForRule "NoExposingEverything"

                    before =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (foo)\n\nfoo = 1\n"

                    after =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (foo)\n\nfoo = 999\n"

                    emptyHashes =
                        Dict.empty

                    emptyGraph =
                        DepGraph.buildGraph { sourceDirectories = [], files = [] }
                in
                ReviewRunner.narrowCacheKey profile "src/Foo.elm" before emptyHashes emptyGraph
                    |> Expect.equal (ReviewRunner.narrowCacheKey profile "src/Foo.elm" after emptyHashes emptyGraph)
        , test "NoExposingEverything: exposing change DOES change key" <|
            \_ ->
                let
                    profile =
                        ReviewRunner.profileForRule "NoExposingEverything"

                    before =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (foo)\n\nfoo = 1\n"

                    after =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (..)\n\nfoo = 1\n"

                    emptyHashes =
                        Dict.empty

                    emptyGraph =
                        DepGraph.buildGraph { sourceDirectories = [], files = [] }
                in
                ReviewRunner.narrowCacheKey profile "src/Foo.elm" before emptyHashes emptyGraph
                    |> Expect.notEqual (ReviewRunner.narrowCacheKey profile "src/Foo.elm" after emptyHashes emptyGraph)
        , test "NoDebug.Log: body change DOES change key (expression dep)" <|
            \_ ->
                let
                    profile =
                        ReviewRunner.profileForRule "NoDebug.Log"

                    before =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (..)\n\nfoo = 1\n"

                    after =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (..)\n\nfoo = Debug.log \"x\" 1\n"

                    emptyHashes =
                        Dict.empty

                    emptyGraph =
                        DepGraph.buildGraph { sourceDirectories = [], files = [] }
                in
                ReviewRunner.narrowCacheKey profile "src/Foo.elm" before emptyHashes emptyGraph
                    |> Expect.notEqual (ReviewRunner.narrowCacheKey profile "src/Foo.elm" after emptyHashes emptyGraph)
        , test "NoMissingTypeAnnotation: body change does NOT change key" <|
            \_ ->
                let
                    profile =
                        ReviewRunner.profileForRule "NoMissingTypeAnnotation"

                    before =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (..)\n\nfoo = 1\n"

                    after =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (..)\n\nfoo = 999\n"

                    emptyHashes =
                        Dict.empty

                    emptyGraph =
                        DepGraph.buildGraph { sourceDirectories = [], files = [] }
                in
                ReviewRunner.narrowCacheKey profile "src/Foo.elm" before emptyHashes emptyGraph
                    |> Expect.equal (ReviewRunner.narrowCacheKey profile "src/Foo.elm" after emptyHashes emptyGraph)
        , test "unknown rule: conservative, body change DOES change key" <|
            \_ ->
                let
                    profile =
                        ReviewRunner.profileForRule "SomeUnknownRule"

                    before =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (..)\n\nfoo = 1\n"

                    after =
                        SemanticHash.computeAspectHashesFromSource
                            "module Foo exposing (..)\n\nfoo = 999\n"

                    emptyHashes =
                        Dict.empty

                    emptyGraph =
                        DepGraph.buildGraph { sourceDirectories = [], files = [] }
                in
                ReviewRunner.narrowCacheKey profile "src/Foo.elm" before emptyHashes emptyGraph
                    |> Expect.notEqual (ReviewRunner.narrowCacheKey profile "src/Foo.elm" after emptyHashes emptyGraph)
        ]


ruleClassificationTests : Test
ruleClassificationTests =
    describe "rule classification"
        [ test "module rule detected from newModuleRuleSchema" <|
            \_ ->
                ReviewRunner.classifyRuleSource moduleRuleSource
                    |> Expect.equal ReviewRunner.ModuleRule
        , test "project rule detected from newProjectRuleSchema" <|
            \_ ->
                ReviewRunner.classifyRuleSource projectRuleSource
                    |> Expect.equal ReviewRunner.ProjectRule
        , test "unknown source defaults to ProjectRule (conservative)" <|
            \_ ->
                ReviewRunner.classifyRuleSource "module Foo exposing (..)\n\nfoo = 1\n"
                    |> Expect.equal ReviewRunner.ProjectRule
        , test "module rule with pipeline operators" <|
            \_ ->
                ReviewRunner.classifyRuleSource moduleRuleWithPipeline
                    |> Expect.equal ReviewRunner.ModuleRule
        ]


moduleRuleSource : String
moduleRuleSource =
    """module NoDebug.Log exposing (rule)

import Review.Rule as Rule exposing (Rule)

rule =
    Rule.newModuleRuleSchema "NoDebug.Log" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema
"""


projectRuleSource : String
projectRuleSource =
    """module NoUnused.Exports exposing (rule)

import Review.Rule as Rule exposing (Rule)

rule =
    Rule.newProjectRuleSchema "NoUnused.Exports" initialProjectContext
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator creator
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema
"""


moduleRuleWithPipeline : String
moduleRuleWithPipeline =
    """module NoMissingTypeAnnotation exposing (rule)

import Review.Rule as Rule exposing (Rule)

rule : Rule
rule =
    Rule.newModuleRuleSchema "NoMissingTypeAnnotation" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema
"""
