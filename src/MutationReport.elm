module MutationReport exposing
    ( FileReport
    , MutantReport
    , MutantStatus(..)
    , toJson
    )

{-| Encode mutation testing results in the Stryker mutation testing report schema.

See: <https://github.com/stryker-mutator/mutation-testing-elements/tree/master/packages/report-schema>

The output JSON can be viewed with the `mutation-testing-elements` web component
(Apache-2.0, fully offline, 224KB).

-}

import Json.Encode as Encode


type alias FileReport =
    { filePath : String
    , sourceCode : String
    , mutants : List MutantReport
    }


type alias MutantReport =
    { id : String
    , status : MutantStatus
    , mutatorName : String
    , description : String
    , location : { start : { line : Int, column : Int }, end_ : { line : Int, column : Int } }
    , replacement : String
    }


type MutantStatus
    = Killed
    | Survived
    | RuntimeError
    | Timeout


{-| Encode mutation results into the Stryker mutation testing report schema (v2).
-}
toJson : { thresholds : { high : Int, low : Int } } -> List FileReport -> Encode.Value
toJson config files =
    Encode.object
        [ ( "schemaVersion", Encode.string "2" )
        , ( "thresholds"
          , Encode.object
                [ ( "high", Encode.int config.thresholds.high )
                , ( "low", Encode.int config.thresholds.low )
                ]
          )
        , ( "files"
          , files
                |> List.map (\file -> ( file.filePath, encodeFile file ))
                |> Encode.object
          )
        ]


encodeFile : FileReport -> Encode.Value
encodeFile file =
    Encode.object
        [ ( "language", Encode.string "elm" )
        , ( "source", Encode.string file.sourceCode )
        , ( "mutants"
          , file.mutants
                |> List.map encodeMutant
                |> Encode.list identity
          )
        ]


encodeMutant : MutantReport -> Encode.Value
encodeMutant mutant =
    Encode.object
        [ ( "id", Encode.string mutant.id )
        , ( "mutatorName", Encode.string mutant.mutatorName )
        , ( "description", Encode.string mutant.description )
        , ( "replacement", Encode.string mutant.replacement )
        , ( "status", Encode.string (statusToString mutant.status) )
        , ( "location"
          , Encode.object
                [ ( "start"
                  , Encode.object
                        [ ( "line", Encode.int mutant.location.start.line )
                        , ( "column", Encode.int mutant.location.start.column )
                        ]
                  )
                , ( "end"
                  , Encode.object
                        [ ( "line", Encode.int mutant.location.end_.line )
                        , ( "column", Encode.int mutant.location.end_.column )
                        ]
                  )
                ]
          )
        ]


statusToString : MutantStatus -> String
statusToString status =
    case status of
        Killed ->
            "Killed"

        Survived ->
            "Survived"

        RuntimeError ->
            "RuntimeError"

        Timeout ->
            "Timeout"
