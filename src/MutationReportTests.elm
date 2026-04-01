module MutationReportTests exposing (suite)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import MutationReport
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "MutationReport"
        [ describe "toJson"
            [ test "produces valid schema version" <|
                \_ ->
                    let
                        json =
                            MutationReport.toJson { thresholds = { high = 80, low = 60 } } []
                    in
                    json
                        |> Decode.decodeValue (Decode.field "schemaVersion" Decode.string)
                        |> Expect.equal (Ok "2")
            , test "includes thresholds" <|
                \_ ->
                    let
                        json =
                            MutationReport.toJson { thresholds = { high = 80, low = 60 } } []
                    in
                    json
                        |> Decode.decodeValue (Decode.at [ "thresholds", "high" ] Decode.int)
                        |> Expect.equal (Ok 80)
            , test "encodes file with source" <|
                \_ ->
                    let
                        json =
                            MutationReport.toJson { thresholds = { high = 80, low = 60 } }
                                [ { filePath = "src/Foo.elm"
                                  , sourceCode = "module Foo exposing (..)\n\nfoo = 1"
                                  , mutants = []
                                  }
                                ]
                    in
                    json
                        |> Decode.decodeValue (Decode.at [ "files", "src/Foo.elm", "source" ] Decode.string)
                        |> Expect.equal (Ok "module Foo exposing (..)\n\nfoo = 1")
            , test "encodes language as elm" <|
                \_ ->
                    let
                        json =
                            MutationReport.toJson { thresholds = { high = 80, low = 60 } }
                                [ { filePath = "src/Foo.elm"
                                  , sourceCode = "module Foo exposing (..)"
                                  , mutants = []
                                  }
                                ]
                    in
                    json
                        |> Decode.decodeValue (Decode.at [ "files", "src/Foo.elm", "language" ] Decode.string)
                        |> Expect.equal (Ok "elm")
            , test "encodes killed mutant" <|
                \_ ->
                    let
                        json =
                            MutationReport.toJson { thresholds = { high = 80, low = 60 } }
                                [ { filePath = "src/Foo.elm"
                                  , sourceCode = "module Foo exposing (..)\n\nfoo = 1"
                                  , mutants =
                                        [ { id = "1"
                                          , status = MutationReport.Killed
                                          , mutatorName = "replaceIntLiteral"
                                          , description = "Changed 1 to 2"
                                          , location = { start = { line = 3, column = 7 }, end_ = { line = 3, column = 8 } }
                                          , replacement = "2"
                                          }
                                        ]
                                  }
                                ]

                        mutantStatus =
                            json
                                |> Decode.decodeValue
                                    (Decode.at [ "files", "src/Foo.elm", "mutants" ]
                                        (Decode.index 0 (Decode.field "status" Decode.string))
                                    )
                    in
                    mutantStatus |> Expect.equal (Ok "Killed")
            , test "encodes survived mutant" <|
                \_ ->
                    let
                        json =
                            MutationReport.toJson { thresholds = { high = 80, low = 60 } }
                                [ { filePath = "src/Foo.elm"
                                  , sourceCode = ""
                                  , mutants =
                                        [ { id = "1"
                                          , status = MutationReport.Survived
                                          , mutatorName = "swapComparison"
                                          , description = "Changed > to >="
                                          , location = { start = { line = 5, column = 10 }, end_ = { line = 5, column = 11 } }
                                          , replacement = ">="
                                          }
                                        ]
                                  }
                                ]

                        mutantStatus =
                            json
                                |> Decode.decodeValue
                                    (Decode.at [ "files", "src/Foo.elm", "mutants" ]
                                        (Decode.index 0 (Decode.field "status" Decode.string))
                                    )
                    in
                    mutantStatus |> Expect.equal (Ok "Survived")
            , test "encodes error mutant as RuntimeError" <|
                \_ ->
                    let
                        json =
                            MutationReport.toJson { thresholds = { high = 80, low = 60 } }
                                [ { filePath = "src/Foo.elm"
                                  , sourceCode = ""
                                  , mutants =
                                        [ { id = "1"
                                          , status = MutationReport.RuntimeError
                                          , mutatorName = "replaceArithmetic"
                                          , description = "Changed + to -"
                                          , location = { start = { line = 4, column = 5 }, end_ = { line = 4, column = 6 } }
                                          , replacement = "-"
                                          }
                                        ]
                                  }
                                ]

                        mutantStatus =
                            json
                                |> Decode.decodeValue
                                    (Decode.at [ "files", "src/Foo.elm", "mutants" ]
                                        (Decode.index 0 (Decode.field "status" Decode.string))
                                    )
                    in
                    mutantStatus |> Expect.equal (Ok "RuntimeError")
            , test "encodes mutant location" <|
                \_ ->
                    let
                        json =
                            MutationReport.toJson { thresholds = { high = 80, low = 60 } }
                                [ { filePath = "src/Foo.elm"
                                  , sourceCode = ""
                                  , mutants =
                                        [ { id = "42"
                                          , status = MutationReport.Killed
                                          , mutatorName = "negateCondition"
                                          , description = "Negated if-condition"
                                          , location = { start = { line = 5, column = 8 }, end_ = { line = 5, column = 13 } }
                                          , replacement = "not (x > 0)"
                                          }
                                        ]
                                  }
                                ]

                        startLine =
                            json
                                |> Decode.decodeValue
                                    (Decode.at [ "files", "src/Foo.elm", "mutants" ]
                                        (Decode.index 0 (Decode.at [ "location", "start", "line" ] Decode.int))
                                    )

                        endColumn =
                            json
                                |> Decode.decodeValue
                                    (Decode.at [ "files", "src/Foo.elm", "mutants" ]
                                        (Decode.index 0 (Decode.at [ "location", "end", "column" ] Decode.int))
                                    )
                    in
                    Expect.all
                        [ \_ -> startLine |> Expect.equal (Ok 5)
                        , \_ -> endColumn |> Expect.equal (Ok 13)
                        ]
                        ()
            , test "encodes mutant replacement text" <|
                \_ ->
                    let
                        json =
                            MutationReport.toJson { thresholds = { high = 80, low = 60 } }
                                [ { filePath = "src/Foo.elm"
                                  , sourceCode = ""
                                  , mutants =
                                        [ { id = "1"
                                          , status = MutationReport.Killed
                                          , mutatorName = "replaceArithmetic"
                                          , description = "Changed + to -"
                                          , location = { start = { line = 4, column = 5 }, end_ = { line = 4, column = 6 } }
                                          , replacement = "-"
                                          }
                                        ]
                                  }
                                ]
                    in
                    json
                        |> Decode.decodeValue
                            (Decode.at [ "files", "src/Foo.elm", "mutants" ]
                                (Decode.index 0 (Decode.field "replacement" Decode.string))
                            )
                        |> Expect.equal (Ok "-")
            ]
        ]
