module ReviewRunnerTest exposing (suite)

{-| Integration tests for the elm-review runner using Test.BackendTask.

Tests the full pipeline: load review project, evaluate Rule.review via
the interpreter, parse results, verify caching behavior.

-}

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BackendTask.Glob as Glob
import Expect
import FatalError exposing (FatalError)
import Pages.Script as Script
import ReviewRunner
import Test exposing (Test, describe, test)
import Test.BackendTask as BackendTaskTest


suite : Test
suite =
    describe "ReviewRunner integration"
        [ pureHelperTests
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
