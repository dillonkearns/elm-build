module UserAccessTests exposing (results, suite)

import Expect
import SimpleTestRunner
import Test exposing (Test, describe, test)
import UserAccess exposing (Role(..))


results : String
results =
    SimpleTestRunner.runToString suite


suite : Test
suite =
    describe "UserAccess"
        [ describe "canView"
            [ test "admin can view" <|
                \_ -> Expect.equal (UserAccess.canView Admin) True
            , test "guest can view" <|
                \_ -> Expect.equal (UserAccess.canView Guest) True
            ]
        , describe "canEdit"
            [ test "admin can edit" <|
                \_ -> Expect.equal (UserAccess.canEdit Admin) True
            , test "member can edit" <|
                \_ -> Expect.equal (UserAccess.canEdit Member) True
            , test "guest cannot edit" <|
                \_ -> Expect.equal (UserAccess.canEdit Guest) False
            ]
        , describe "canDelete"
            [ test "admin can delete" <|
                \_ -> Expect.equal (UserAccess.canDelete Admin) True
            , test "member cannot delete" <|
                \_ -> Expect.equal (UserAccess.canDelete Member) False
            ]
        , describe "ageGroup"
            [ test "child" <|
                \_ -> Expect.equal (UserAccess.ageGroup 8) "child"
            , test "teen" <|
                \_ -> Expect.equal (UserAccess.ageGroup 15) "teen"
            , test "adult" <|
                \_ -> Expect.equal (UserAccess.ageGroup 30) "adult"
            , test "senior" <|
                \_ -> Expect.equal (UserAccess.ageGroup 70) "senior"
            ]
        ]
