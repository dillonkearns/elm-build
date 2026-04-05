module ReviewRunnerTest exposing (suite)

{-| Integration tests for the elm-review runner using Test.BackendTask.

Tests the full pipeline: load review project, evaluate Rule.review via
the interpreter, parse results, verify caching behavior.

-}

import Dict
import Expect
import ReviewRunner
import Test exposing (Test, describe, test)
import Test.BackendTask as BackendTaskTest


suite : Test
suite =
    describe "ReviewRunner integration"
        [ pureHelperTests
        , semanticCacheKeyTests
        , perDeclarationTests
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
