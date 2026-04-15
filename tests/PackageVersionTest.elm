module PackageVersionTest exposing (suite)

import Expect
import PackageVersion
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "PackageVersion.chooseLatestCompatibleVersion"
        [ test "sorts semver numerically instead of lexicographically" <|
            \_ ->
                PackageVersion.chooseLatestCompatibleVersion
                    "2.15.5 <= v < 3.0.0"
                    [ "2.6.1", "2.16.6", "2.15.1" ]
                    |> Expect.equal (Just "2.16.6")
        , test "respects the upper bound of a package range" <|
            \_ ->
                PackageVersion.chooseLatestCompatibleVersion
                    "1.0.0 <= v < 2.0.0"
                    [ "1.2.2", "2.2.1" ]
                    |> Expect.equal (Just "1.2.2")
        , test "empty constraint picks the numerically latest installed version" <|
            \_ ->
                PackageVersion.chooseLatestCompatibleVersion
                    ""
                    [ "1.0.0", "2.6.1", "2.16.6" ]
                    |> Expect.equal (Just "2.16.6")
        , test "returns Nothing when no installed version satisfies the constraint" <|
            \_ ->
                PackageVersion.chooseLatestCompatibleVersion
                    "3.0.0 <= v < 4.0.0"
                    [ "1.0.0", "2.16.6" ]
                    |> Expect.equal Nothing
        ]
