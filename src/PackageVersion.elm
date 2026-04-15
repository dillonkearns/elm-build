module PackageVersion exposing (chooseLatestCompatibleVersion)

{-| Select the numerically highest installed package version that satisfies
an Elm package version range such as `1.0.0 <= v < 2.0.0`.
-}


chooseLatestCompatibleVersion : String -> List String -> Maybe String
chooseLatestCompatibleVersion versionOrRange installedVersions =
    installedVersions
        |> List.filter (versionMatchesConstraint versionOrRange)
        |> List.sortWith compareVersionStrings
        |> List.reverse
        |> List.head


versionMatchesConstraint : String -> String -> Bool
versionMatchesConstraint versionOrRange version =
    if String.isEmpty versionOrRange then
        True

    else
        case String.words versionOrRange of
            [ lower, "<=", "v", "<", upper ] ->
                compareVersionStrings version lower /= LT
                    && compareVersionStrings version upper == LT

            [ lower, "<=", "v", "<=", upper ] ->
                compareVersionStrings version lower /= LT
                    && compareVersionStrings version upper /= GT

            _ ->
                compareVersionStrings version versionOrRange == EQ


compareVersionStrings : String -> String -> Order
compareVersionStrings left right =
    case ( parseVersion left, parseVersion right ) of
        ( Just leftVersion, Just rightVersion ) ->
            compare leftVersion rightVersion

        ( Just _, Nothing ) ->
            GT

        ( Nothing, Just _ ) ->
            LT

        ( Nothing, Nothing ) ->
            compare left right


parseVersion : String -> Maybe ( Int, Int, Int )
parseVersion version =
    case String.split "." version of
        [ major, minor, patch ] ->
            Maybe.map3
                (\parsedMajor parsedMinor parsedPatch ->
                    ( parsedMajor, parsedMinor, parsedPatch )
                )
                (String.toInt major)
                (String.toInt minor)
                (String.toInt patch)

        _ ->
            Nothing
