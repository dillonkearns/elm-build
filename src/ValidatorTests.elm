module ValidatorTests exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Validator exposing (PasswordStrength(..))


suite : Test
suite =
    describe "Validator"
        [ describe "validateUsername"
            [ test "rejects short username" <|
                \_ ->
                    Validator.validateUsername "ab"
                        |> Expect.err
            , test "accepts valid username" <|
                \_ ->
                    Validator.validateUsername "alice"
                        |> Expect.ok
            , test "rejects too-long username" <|
                \_ ->
                    Validator.validateUsername "aaaaabbbbbcccccddddde"
                        |> Expect.err
            ]
        , describe "validateEmail"
            [ test "accepts email with @" <|
                \_ ->
                    Validator.validateEmail "user@example.com"
                        |> Expect.ok
            , test "rejects email without @" <|
                \_ ->
                    Validator.validateEmail "not-an-email"
                        |> Expect.err
            ]
        , describe "validateAge"
            [ test "rejects child" <|
                \_ ->
                    Validator.validateAge 10
                        |> Expect.err
            , test "accepts adult" <|
                \_ ->
                    Validator.validateAge 25
                        |> Expect.ok
            , test "rejects unreasonable age" <|
                \_ ->
                    Validator.validateAge 150
                        |> Expect.err
            ]
        , describe "passwordStrength"
            [ test "short password is weak" <|
                \_ ->
                    Validator.passwordStrength "abc"
                        |> Expect.equal Weak
            , test "medium password is fair" <|
                \_ ->
                    Validator.passwordStrength "password1"
                        |> Expect.equal Fair
            , test "long password is strong" <|
                \_ ->
                    Validator.passwordStrength "correcthorsebattery"
                        |> Expect.equal Strong
            ]
        ]
