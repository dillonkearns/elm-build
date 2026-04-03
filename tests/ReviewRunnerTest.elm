module ReviewRunnerTest exposing (suite)

import Expect
import ReviewRunner
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "ReviewRunner"
        [ escapeElmStringTests
        , buildModuleRecordTests
        , buildExpressionTests
        , parseReviewOutputTests
        ]


escapeElmStringTests : Test
escapeElmStringTests =
    describe "escapeElmString"
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


buildModuleRecordTests : Test
buildModuleRecordTests =
    describe "buildModuleRecord"
        [ test "simple module" <|
            \_ ->
                ReviewRunner.buildModuleRecord { path = "src/Foo.elm", source = "module Foo exposing (..)\n\na = 1" }
                    |> Expect.equal "{ path = \"src/Foo.elm\", source = \"module Foo exposing (..)\\n\\na = 1\" }"
        , test "module with quotes in source" <|
            \_ ->
                ReviewRunner.buildModuleRecord { path = "src/Bar.elm", source = "x = \"hello\"" }
                    |> Expect.equal "{ path = \"src/Bar.elm\", source = \"x = \\\"hello\\\"\" }"
        ]


buildExpressionTests : Test
buildExpressionTests =
    describe "buildExpression"
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


parseReviewOutputTests : Test
parseReviewOutputTests =
    describe "parseReviewOutput"
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
