module SimpleSampleTests exposing (results)

{-| Equivalent to SampleTests but using only core Elm — no test framework.

This module can be evaluated by the Elm interpreter since it only depends
on Basics, List, String, and other core modules.

-}


results : String
results =
    let
        tests : List ( String, Bool )
        tests =
            [ -- Math
              ( "Sample Tests > Math > addition", (1 + 1) == 2 )
            , ( "Sample Tests > Math > multiplication", (3 * 4) == 12 )
            , ( "Sample Tests > Math > negative", negate 5 == -5 )

            -- Strings
            , ( "Sample Tests > Strings > reverse", String.reverse "hello" == "olleh" )
            , ( "Sample Tests > Strings > length", String.length "elm" == 3 )
            , ( "Sample Tests > Strings > concat", String.append "foo" "bar" == "foobar" )

            -- Lists
            , ( "Sample Tests > Lists > head", List.head [ 1, 2, 3 ] == Just 1 )
            , ( "Sample Tests > Lists > length", List.length [ 1, 2, 3 ] == 3 )
            , ( "Sample Tests > Lists > reverse", List.reverse [ 1, 2, 3 ] == [ 3, 2, 1 ] )

            ]

        passCount : Int
        passCount =
            List.length (List.filter Tuple.second tests)

        failCount : Int
        failCount =
            List.length tests - passCount

        formatTest : ( String, Bool ) -> String
        formatTest ( name, passed ) =
            if passed then
                "PASS:" ++ name

            else
                "FAIL:" ++ name
    in
    String.fromInt passCount
        ++ ","
        ++ String.fromInt failCount
        ++ ","
        ++ String.fromInt (List.length tests)
        ++ "\n"
        ++ (List.map formatTest tests |> String.join "\n")
