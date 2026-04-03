module MutationTestRunner exposing (run)

{-| Mutation testing runner that uses the elm-interpreter via InterpreterProject.

For each mutation, swaps the mutated source into the interpreter evaluation
via `evalWithFileOverrides`. Results are cached via elm-build's
content-addressed Cache, so re-runs with unchanged code are instant.

The test module just needs to expose `suite : Test` — the standard elm-test
convention. Test values are auto-discovered via the interpreter.

-}

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Glob as Glob
import BackendTask.Time
import Cache
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import DepGraph
import Elm.Syntax.Expression exposing (Expression(..))
import Eval.Module
import FatalError exposing (FatalError)
import Coverage
import Elm.Syntax.Range exposing (Range)
import InterpreterProject exposing (InterpreterProject)
import Json.Encode
import MutationReport
import Mutator exposing (Mutation)
import Pages.Script as Script exposing (Script)
import Path exposing (Path)
import Set
import TestAnalysis
import Time
import Types


type alias Config =
    { mutateFile : Maybe String
    , testFile : Maybe String
    , suiteName : String
    , sourceDirs : List String
    , buildDirectory : Path
    , verbose : Bool
    , breakThreshold : Maybe Int
    , excludeOperators : List String
    , onlyOperators : List String
    , reportFile : Maybe String
    }


programConfig : Program.Config Config
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build Config
                |> OptionsParser.with
                    (Option.optionalKeywordArg "mutate"
                        |> Option.withDescription "Source file to mutate (omit to auto-discover from test imports)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "test"
                        |> Option.withDescription "The test file (auto-discovered if omitted)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "suite"
                        |> Option.map (Maybe.withDefault "suite")
                        |> Option.withDescription "The name of the test value (default: suite)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "source-dirs"
                        |> Option.map (Maybe.map (String.split ",") >> Maybe.withDefault [])
                        |> Option.withDescription "Extra source directories (comma-separated)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "build"
                        |> Option.map (Maybe.withDefault ".elm-mutation-test" >> Path.path)
                        |> Option.withDescription "Build/cache directory (default: .elm-mutation-test)"
                    )
                |> OptionsParser.with
                    (Option.flag "verbose"
                        |> Option.withDescription "Show killed mutants (default: only show survivors)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "break"
                        |> Option.map (Maybe.andThen String.toInt)
                        |> Option.withDescription "Fail if mutation score is below this percentage"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "exclude"
                        |> Option.map (Maybe.map (String.split ",") >> Maybe.withDefault [])
                        |> Option.withDescription "Exclude mutation operators (comma-separated)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "only"
                        |> Option.map (Maybe.map (String.split ",") >> Maybe.withDefault [])
                        |> Option.withDescription "Only run these mutation operators (comma-separated)"
                    )
                |> OptionsParser.with
                    (Option.optionalKeywordArg "report"
                        |> Option.withDescription "Write mutation testing report JSON to this path"
                    )
            )


run : Script
run =
    Script.withCliOptions programConfig (task >> BackendTask.quiet)


task : Config -> BackendTask FatalError ()
task config =
    BackendTask.Extra.profiling "mutation-test-runner" <|
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue "Loading project sources for mutation testing") <| \_ ->
        -- Ensure dependencies are fetched into ELM_HOME
        Do.do (ensureDependenciesFetched config) <| \_ ->
        let
            sourceDirectories =
                "src" :: config.sourceDirs

            allDirectories =
                "tests" :: sourceDirectories
        in
        -- Load the project via InterpreterProject (packages parsed once, cached)
        Do.do
            (BackendTask.Extra.timed "Loading project" "Loaded project"
                (InterpreterProject.loadWith
                    { projectDir = Path.path "."
                    , skipPackages = kernelPackages
                    , patchSource = patchSource
                    , extraSourceFiles = []
                    , sourceDirectories = Just allDirectories
                    }
                )
            )
        <| \project ->
        -- Resolve the test file: either explicitly provided or auto-discovered
        Do.do (resolveTestFile config project) <| \testFile ->
        Do.allowFatal (File.rawFile testFile) <| \testSource ->
        let
            testModuleName : String
            testModuleName =
                DepGraph.parseModuleName testSource
                    |> Maybe.withDefault "Tests"

            candidateNames : List String
            candidateNames =
                TestAnalysis.getCandidateNames testSource
        in
        Do.log ("Found " ++ String.fromInt (List.length candidateNames) ++ " candidate values in " ++ testModuleName) <| \_ ->
        -- Discover which candidates are actually Test values via the interpreter
        let
            packageEnv =
                InterpreterProject.getPackageEnv project

            -- Get user sources needed for probing (test module + its deps)
            probeSources =
                let
                    { userSources } =
                        InterpreterProject.prepareEvalSources project
                            { imports = [ "SimpleTestRunner", testModuleName ]
                            , expression = "\"probe\""
                            }
                in
                simpleTestRunnerSource :: userSources
        in
        Do.do
            (BackendTask.Extra.timed "Discovering test values" "Discovered test values"
                (let
                    probeResults =
                        candidateNames
                            |> List.map
                                (\name ->
                                    ( name, TestAnalysis.probeCandidate packageEnv testModuleName name probeSources )
                                )

                    testValues =
                        probeResults
                            |> List.filterMap
                                (\( name, result ) ->
                                    case result of
                                        Ok _ ->
                                            Just name

                                        Err _ ->
                                            Nothing
                                )

                    rejections =
                        probeResults
                            |> List.filterMap
                                (\( name, result ) ->
                                    case result of
                                        Err reason ->
                                            Just (testModuleName ++ "." ++ name ++ ": " ++ reason)

                                        Ok _ ->
                                            Nothing
                                )
                 in
                 if List.isEmpty testValues then
                    BackendTask.fail
                        (FatalError.fromString
                            ("No Test values found in "
                                ++ testModuleName
                                ++ ".\nCandidates rejected:\n  "
                                ++ String.join "\n  " rejections
                            )
                        )

                 else
                    (if List.isEmpty rejections then
                        BackendTask.succeed ()

                     else
                        Script.log ("Rejected candidates: " ++ String.join ", " rejections)
                    )
                        |> BackendTask.map (\() -> testValues)
                )
            )
        <| \testValues ->
        let
            suiteExpr : String
            suiteExpr =
                case testValues of
                    [ single ] ->
                        testModuleName ++ "." ++ single

                    multiple ->
                        "Test.describe \"" ++ testModuleName ++ "\" [" ++ String.join ", " (List.map (\v -> testModuleName ++ "." ++ v) multiple) ++ "]"

            evalConfig : { imports : List String, expression : String }
            evalConfig =
                { imports = [ "SimpleTestRunner", "Test", testModuleName ]
                , expression = "SimpleTestRunner.runToString (" ++ suiteExpr ++ ")"
                }
        in
        Do.log ("Discovered test values: " ++ String.join ", " (List.map (\v -> testModuleName ++ "." ++ v) testValues)) <| \_ ->
        -- Determine which files to mutate
        Do.do (resolveFilesToMutate config project sourceDirectories testFile) <| \filesToMutate ->
        Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue ("Mutating " ++ String.fromInt (List.length filesToMutate) ++ " source file(s): " ++ String.join ", " filesToMutate)) <| \_ ->
        -- Baseline: run the full suite unmutated to get the reference output
        Do.do
            (InterpreterProject.evalWithCoverage project
                { imports = evalConfig.imports
                , expression = evalConfig.expression
                , sourceOverrides = [ simpleTestRunnerSource ]
                }
            )
        <| \baselineCoverage ->
        let
            baselineOutput =
                baselineCoverage.result
        in
        -- Per-test coverage: run each test individually with tracing
        Do.do
            (BackendTask.Extra.timed "Collecting per-test coverage" "Coverage collected"
                (testValues
                    |> List.map
                        (\testValue ->
                            let
                                singleTestExpr =
                                    "SimpleTestRunner.runToString (" ++ testModuleName ++ "." ++ testValue ++ ")"
                            in
                            InterpreterProject.evalWithCoverage project
                                { imports = evalConfig.imports
                                , expression = singleTestExpr
                                , sourceOverrides = [ simpleTestRunnerSource ]
                                }
                                |> BackendTask.map
                                    (\result ->
                                        { testName = testModuleName ++ "." ++ testValue
                                        , coveredRanges = result.coveredRanges
                                        , baselineResult = result.result
                                        }
                                    )
                        )
                    |> BackendTask.Extra.combine
                )
            )
        <| \perTestCoverage ->
        let
            -- Flatten all covered ranges (union across all tests)
            allCoveredRanges =
                perTestCoverage |> List.concatMap .coveredRanges

            totalCoveredExpressions =
                List.length allCoveredRanges

            -- Note: per-test baselines are available in perTestCoverage but can't
            -- be directly compared with mutation outputs (different suite composition).
            -- Equivalent detection works when mutations run the full suite.
        in
        Do.log ("  " ++ String.fromInt (List.length perTestCoverage) ++ " tests, " ++ String.fromInt totalCoveredExpressions ++ " covered expression ranges") <| \_ ->
        Do.exec "mkdir" [ "-p", Path.toString config.buildDirectory ] <| \_ ->
        -- Pre-compute the cache directory listing once, instead of per-mutation
        Do.do (Cache.listExisting config.buildDirectory) <| \existingCache ->
        Do.do BackendTask.Time.now <| \startTime ->
        -- Run mutations for each file and collect per-file results
        Do.do
            (filesToMutate
                |> BackendTask.Extra.mapSequence
                    (\mutateFilePath ->
                        Do.allowFatal (File.rawFile mutateFilePath) <| \mutateSource ->
                        let
                            allMutations =
                                Mutator.generateMutations mutateSource

                            mutations =
                                filterMutations config allMutations

                            coveredMutations =
                                mutations
                                    |> List.filter (\m -> Coverage.isCovered allCoveredRanges m.spliceRange)

                            uncoveredMutations =
                                mutations
                                    |> List.filter (\m -> not (Coverage.isCovered allCoveredRanges m.spliceRange))

                            uncoveredResults =
                                uncoveredMutations
                                    |> List.map (\m -> NoCoverageResult { mutation = m })
                        in
                        Do.log ("  " ++ mutateFilePath ++ ": " ++ String.fromInt (List.length mutations) ++ " mutations (" ++ String.fromInt (List.length coveredMutations) ++ " covered, " ++ String.fromInt (List.length uncoveredMutations) ++ " no coverage)") <| \_ ->
                        Do.do
                            (coveredMutations
                                |> List.indexedMap Tuple.pair
                                |> BackendTask.Extra.mapSequence
                                    (\( mutIndex, mutation ) ->
                                        let
                                            mutationProgress =
                                                "    [" ++ String.fromInt (mutIndex + 1) ++ "/" ++ String.fromInt (List.length coveredMutations) ++ "] " ++ mutation.operator ++ " " ++ mutateFilePath ++ ":" ++ String.fromInt mutation.line

                                            -- Find tests whose coverage includes this mutation's range
                                            relevantTests =
                                                perTestCoverage
                                                    |> List.filter (\tc -> Coverage.isCovered tc.coveredRanges mutation.spliceRange)
                                                    |> List.map .testName

                                            -- Build a suite expression with only the relevant tests
                                            isFullSuite =
                                                List.length relevantTests == List.length perTestCoverage

                                            relevantSuiteExpr =
                                                if isFullSuite then
                                                    evalConfig.expression

                                                else
                                                    case relevantTests of
                                                        [ single ] ->
                                                            "SimpleTestRunner.runToString (" ++ single ++ ")"

                                                        multiple ->
                                                            "SimpleTestRunner.runToString (Test.describe \"relevant\" [" ++ String.join ", " multiple ++ "])"

                                            -- For equivalent detection: compare mutation output
                                            -- against the unmutated baseline
                                            relevantBaseline =
                                                if isFullSuite then
                                                    baselineOutput

                                                else
                                                    -- Subset runs have different output format;
                                                    -- can't compare directly. Skip equiv detection.
                                                    ""
                                        in
                                        Do.do
                                            (Cache.runWith { jobs = Nothing, existing = existingCache } config.buildDirectory
                                                (InterpreterProject.evalWithFileOverrides project
                                                    { imports = evalConfig.imports
                                                    , expression = relevantSuiteExpr
                                                    , sourceOverrides = [ simpleTestRunnerSource ]
                                                    , fileOverrides = [ { file = mutation.mutatedFile, hashKey = Mutator.hashKey mutation } ]
                                                    }
                                                    Cache.succeed
                                                )
                                            )
                                        <| \result ->
                                        Do.allowFatal (File.rawFile (Path.toString result.output)) <| \output ->
                                        let
                                            mutResult =
                                                parseMutationResult relevantBaseline mutation output

                                            statusStr =
                                                case mutResult of
                                                    Killed _ ->
                                                        Ansi.Color.fontColor Ansi.Color.green "KILLED"

                                                    Survived _ ->
                                                        Ansi.Color.fontColor Ansi.Color.red "SURVIVED"

                                                    EquivalentResult _ ->
                                                        Ansi.Color.fontColor Ansi.Color.yellow "EQUIVALENT"

                                                    NoCoverageResult _ ->
                                                        "NO COVERAGE"

                                                    ErrorResult _ ->
                                                        Ansi.Color.fontColor Ansi.Color.yellow "ERROR"
                                        in
                                        Do.log (mutationProgress ++ " " ++ statusStr) <| \_ ->
                                        BackendTask.succeed mutResult
                                    )
                            )
                        <| \coveredResults ->
                        BackendTask.succeed
                            { filePath = mutateFilePath
                            , sourceCode = mutateSource
                            , results = coveredResults ++ uncoveredResults
                            }
                    )
            )
        <| \allFileResults ->
        Do.do BackendTask.Time.now <| \endTime ->
        let
            totalMutations =
                allFileResults |> List.map (.results >> List.length) |> List.sum

            evalMs =
                Time.posixToMillis endTime - Time.posixToMillis startTime

            perMutation =
                if totalMutations > 0 then
                    evalMs // totalMutations

                else
                    0
        in
        Do.log
            ("Evaluated "
                ++ String.fromInt totalMutations
                ++ " mutations in "
                ++ String.fromInt evalMs
                ++ "ms ("
                ++ String.fromInt perMutation
                ++ "ms/mutation)"
            )
        <| \_ ->
        Do.do (writeReportFile config allFileResults) <| \_ ->
        displayMultiFileReport config allFileResults


{-| Determine which source files to mutate.

  - If --mutate is provided, use that single file.
  - Otherwise, use the dep graph to find all source files the test imports.

-}
resolveFilesToMutate : Config -> InterpreterProject -> List String -> String -> BackendTask FatalError (List String)
resolveFilesToMutate config project sourceDirectories testFile =
    case config.mutateFile of
        Just singleFile ->
            BackendTask.succeed [ singleFile ]

        Nothing ->
            let
                depGraph =
                    InterpreterProject.getDepGraph project

                sources =
                    DepGraph.sourcesTestedBy sourceDirectories depGraph testFile
            in
            if List.isEmpty sources then
                BackendTask.fail
                    (FatalError.fromString
                        ("No source files found to mutate. The test file "
                            ++ testFile
                            ++ " does not import any modules from: "
                            ++ String.join ", " sourceDirectories
                        )
                    )

            else
                BackendTask.succeed sources


filterMutations : Config -> List Mutation -> List Mutation
filterMutations config mutations =
    mutations
        |> (\ms ->
                if List.isEmpty config.onlyOperators then
                    ms

                else
                    List.filter (\m -> List.member m.operator config.onlyOperators) ms
           )
        |> (\ms ->
                if List.isEmpty config.excludeOperators then
                    ms

                else
                    List.filter (\m -> not (List.member m.operator config.excludeOperators)) ms
           )


parseMutationResult : String -> Mutation -> String -> MutationResult
parseMutationResult baselineOutput mutation output =
    if String.startsWith "ERROR:" output then
        ErrorResult { mutation = mutation, error = String.dropLeft 7 output }

    else if output == baselineOutput then
        -- Early cutoff: mutation produces identical output to unmutated code.
        -- This is a provably equivalent mutation — syntactically different but
        -- semantically identical. No need to propagate to dependents.
        EquivalentResult { mutation = mutation }

    else
        case String.split "," (String.lines output |> List.head |> Maybe.withDefault "") of
            [ _, failStr, _ ] ->
                case String.toInt failStr of
                    Just failCount ->
                        if failCount > 0 then
                            Killed { mutation = mutation, failCount = failCount }

                        else
                            Survived { mutation = mutation }

                    Nothing ->
                        ErrorResult { mutation = mutation, error = "Could not parse fail count" }

            _ ->
                ErrorResult { mutation = mutation, error = "Unexpected output format" }


type MutationResult
    = Killed { mutation : Mutation, failCount : Int }
    | Survived { mutation : Mutation }
    | EquivalentResult { mutation : Mutation }
    | NoCoverageResult { mutation : Mutation }
    | ErrorResult { mutation : Mutation, error : String }


type alias FileResults =
    { filePath : String
    , sourceCode : String
    , results : List MutationResult
    }


{-| Write mutation testing report JSON to disk if --report is specified.
-}
writeReportFile : Config -> List FileResults -> BackendTask FatalError ()
writeReportFile config allFileResults =
    case config.reportFile of
        Nothing ->
            BackendTask.succeed ()

        Just reportPath ->
            let
                reportJson =
                    MutationReport.toJson
                        { thresholds = { high = 80, low = 60 } }
                        (List.map toFileReport allFileResults)

                jsonString =
                    Json.Encode.encode 2 reportJson
            in
            Do.do
                (File.rawFile reportPath
                    |> BackendTask.allowFatal
                    |> BackendTask.onError (\_ -> BackendTask.succeed "")
                    |> BackendTask.andThen (\_ -> BackendTask.succeed ())
                )
            <| \_ ->
            Do.exec "mkdir" [ "-p", reportPath |> String.split "/" |> List.reverse |> List.drop 1 |> List.reverse |> String.join "/" ] <| \_ ->
            Script.writeFile
                { path = reportPath
                , body = jsonString
                }
                |> BackendTask.allowFatal
                |> BackendTask.andThen
                    (\() ->
                        Script.log ("Report written to " ++ reportPath)
                    )


{-| Convert internal FileResults to MutationReport.FileReport.
-}
toFileReport : FileResults -> MutationReport.FileReport
toFileReport fileResults =
    { filePath = fileResults.filePath
    , sourceCode = fileResults.sourceCode
    , mutants =
        fileResults.results
            |> List.indexedMap toMutantReport
    }


toMutantReport : Int -> MutationResult -> MutationReport.MutantReport
toMutantReport index result =
    let
        mutation =
            case result of
                Killed r ->
                    r.mutation

                Survived r ->
                    r.mutation

                EquivalentResult r ->
                    r.mutation

                NoCoverageResult r ->
                    r.mutation

                ErrorResult r ->
                    r.mutation

        status =
            case result of
                Killed _ ->
                    MutationReport.Killed

                Survived _ ->
                    MutationReport.Survived

                EquivalentResult _ ->
                    MutationReport.Equivalent

                NoCoverageResult _ ->
                    MutationReport.NoCoverage

                ErrorResult _ ->
                    MutationReport.RuntimeError
    in
    { id = String.fromInt (index + 1)
    , status = status
    , mutatorName = mutation.operator
    , description = mutation.description
    , location =
        { start = { line = mutation.spliceRange.start.row, column = mutation.spliceRange.start.column }
        , end_ = { line = mutation.spliceRange.end.row, column = mutation.spliceRange.end.column }
        }
    , replacement = mutation.spliceText
    }



displayMultiFileReport : Config -> List FileResults -> BackendTask FatalError ()
displayMultiFileReport config allFileResults =
    let
        multiFile =
            List.length allFileResults > 1
    in
    -- Display per-file details
    Do.each allFileResults
        (\fileResult ->
            let
                killed =
                    List.filter isKilled fileResult.results

                survived =
                    List.filter isSurvived fileResult.results

                errors =
                    List.filter isError fileResult.results

                total =
                    List.length fileResult.results
            in
            Do.do
                (if multiFile then
                    Do.log (Ansi.Color.fontColor Ansi.Color.brightBlue ("\n── " ++ fileResult.filePath ++ " ──")) <| \_ ->
                    BackendTask.succeed ()

                 else
                    BackendTask.succeed ()
                )
            <| \_ ->
            Do.do
                (if config.verbose then
                    Do.each killed
                        (\r ->
                            case r of
                                Killed { mutation, failCount } ->
                                    Script.log
                                        (Ansi.Color.fontColor Ansi.Color.green
                                            ("  ✓ Killed (" ++ String.fromInt failCount ++ " failed): " ++ mutation.description ++ " (line " ++ String.fromInt mutation.line ++ ")")
                                        )

                                _ ->
                                    Script.log ""
                        )
                        (\_ -> BackendTask.succeed ())

                 else
                    BackendTask.succeed ()
                )
            <| \_ ->
            Do.each survived
                (\r ->
                    case r of
                        Survived { mutation } ->
                            Script.log
                                (Ansi.Color.fontColor Ansi.Color.red
                                    ("  ✗ Survived: "
                                        ++ mutation.description
                                        ++ " ("
                                        ++ fileResult.filePath
                                        ++ ":"
                                        ++ String.fromInt mutation.line
                                        ++ ")"
                                    )
                                    ++ "\n"
                                    ++ sourceContext fileResult.sourceCode mutation.line
                                )

                        _ ->
                            Script.log ""
                )
            <| \_ ->
            Do.each errors
                (\r ->
                    case r of
                        ErrorResult { mutation, error } ->
                            Script.log
                                (Ansi.Color.fontColor Ansi.Color.yellow
                                    ("  ? Error: " ++ mutation.description ++ " — " ++ error)
                                )

                        _ ->
                            Script.log ""
                )
            <| \_ ->
            if multiFile then
                let
                    fileScore =
                        if total == 0 then
                            "N/A"

                        else
                            String.fromInt (round (toFloat (List.length killed) / toFloat total * 100)) ++ "%"

                    fileColor =
                        if List.isEmpty survived && List.isEmpty errors then
                            Ansi.Color.fontColor Ansi.Color.green

                        else
                            Ansi.Color.fontColor Ansi.Color.red
                in
                Script.log
                    (fileColor
                        ("  Score: "
                            ++ fileScore
                            ++ " ("
                            ++ String.fromInt (List.length killed)
                            ++ "/"
                            ++ String.fromInt total
                            ++ " killed)"
                        )
                    )

            else
                BackendTask.succeed ()
        )
    <| \_ ->
    -- Overall summary
    let
        allResults =
            allFileResults |> List.concatMap .results

        totalKilled =
            List.filter isKilled allResults |> List.length

        totalSurvived =
            List.filter isSurvived allResults |> List.length

        totalEquivalent =
            List.filter isEquivalent allResults |> List.length

        totalNoCoverage =
            List.filter isNoCoverage allResults |> List.length

        totalErrors =
            List.filter isError allResults |> List.length

        total =
            List.length allResults

        -- Score excludes equivalent + NoCoverage from denominator
        -- (equivalent can't be caught; uncovered can't be reached)
        actionableTotal =
            total - totalEquivalent - totalNoCoverage

        score =
            if actionableTotal == 0 then
                "N/A"

            else
                String.fromInt (round (toFloat totalKilled / toFloat actionableTotal * 100)) ++ "%"

        summaryColor =
            if totalSurvived == 0 && totalErrors == 0 then
                Ansi.Color.fontColor Ansi.Color.brightGreen

            else
                Ansi.Color.fontColor Ansi.Color.brightRed
    in
    Do.log
        (summaryColor
            ("\nMutation Score: "
                ++ score
                ++ " ("
                ++ String.fromInt totalKilled
                ++ " killed, "
                ++ String.fromInt totalSurvived
                ++ " survived, "
                ++ String.fromInt totalEquivalent
                ++ " equivalent, "
                ++ String.fromInt totalNoCoverage
                ++ " no coverage, "
                ++ String.fromInt totalErrors
                ++ " errors, "
                ++ String.fromInt total
                ++ " total)"
            )
        )
    <| \_ ->
    -- Per-file breakdown (only in multi-file mode)
    Do.do
        (if multiFile then
            Do.log "\nPer-file breakdown:" <| \_ ->
            Do.each allFileResults
                (\fileResult ->
                    let
                        fKilled =
                            List.filter isKilled fileResult.results |> List.length

                        fTotal =
                            List.length fileResult.results

                        fScore =
                            if fTotal == 0 then
                                "N/A"

                            else
                                String.fromInt (round (toFloat fKilled / toFloat fTotal * 100)) ++ "%"

                        fColor =
                            if fKilled == fTotal then
                                Ansi.Color.fontColor Ansi.Color.green

                            else
                                Ansi.Color.fontColor Ansi.Color.red
                    in
                    Script.log (fColor ("  " ++ fileResult.filePath ++ ": " ++ fScore ++ " (" ++ String.fromInt fKilled ++ "/" ++ String.fromInt fTotal ++ ")"))
                )
                (\_ -> BackendTask.succeed ())

         else
            BackendTask.succeed ()
        )
    <| \_ ->
    let
        scoreInt =
            if actionableTotal == 0 then
                100

            else
                round (toFloat totalKilled / toFloat actionableTotal * 100)

        shouldFail =
            case config.breakThreshold of
                Just threshold ->
                    scoreInt < threshold

                Nothing ->
                    totalSurvived > 0 || totalErrors > 0
    in
    if shouldFail then
        BackendTask.fail
            (FatalError.build
                { title = "Mutation testing incomplete"
                , body =
                    "Score: "
                        ++ String.fromInt scoreInt
                        ++ "%"
                        ++ (case config.breakThreshold of
                                Just threshold ->
                                    " (threshold: " ++ String.fromInt threshold ++ "%)"

                                Nothing ->
                                    ""
                           )
                }
            )

    else
        Do.noop


isKilled : MutationResult -> Bool
isKilled r =
    case r of
        Killed _ ->
            True

        _ ->
            False


isSurvived : MutationResult -> Bool
isSurvived r =
    case r of
        Survived _ ->
            True

        _ ->
            False


isEquivalent : MutationResult -> Bool
isEquivalent r =
    case r of
        EquivalentResult _ ->
            True

        _ ->
            False


isNoCoverage : MutationResult -> Bool
isNoCoverage r =
    case r of
        NoCoverageResult _ ->
            True

        _ ->
            False


isError : MutationResult -> Bool
isError r =
    case r of
        ErrorResult _ ->
            True

        _ ->
            False


{-| The SimpleTestRunner module source, embedded so it's available regardless
of what project directory the tool runs from.
-}
simpleTestRunnerSource : String
simpleTestRunnerSource =
    String.join "\n"
        [ "module SimpleTestRunner exposing (runToString)"
        , "import Expect"
        , "import Random"
        , "import Test exposing (Test)"
        , "import Test.Runner"
        , "runToString suite ="
        , "    let"
        , "        runners = case Test.Runner.fromTest 1 (Random.initialSeed 42) suite of"
        , "            Test.Runner.Plain list -> Ok list"
        , "            Test.Runner.Only list -> Ok list"
        , "            Test.Runner.Skipping list -> Ok list"
        , "            Test.Runner.Invalid msg -> Err msg"
        , "    in"
        , "    case runners of"
        , "        Err msg -> \"0,1,1\\nFAIL:Invalid test suite: \" ++ msg"
        , "        Ok runnerList ->"
        , "            let"
        , "                results = List.map runOneRunner runnerList"
        , "                passCount = List.length (List.filter .passed results)"
        , "                failCount = List.length results - passCount"
        , "                formatResult r = if r.passed then \"PASS:\" ++ r.label else \"FAIL:\" ++ r.label ++ \" | \" ++ r.message"
        , "            in"
        , "            String.fromInt passCount ++ \",\" ++ String.fromInt failCount ++ \",\" ++ String.fromInt (List.length results) ++ \"\\n\" ++ (List.map formatResult results |> String.join \"\\n\")"
        , "runOneRunner runner ="
        , "    let"
        , "        labelPath = List.reverse runner.labels |> String.join \" > \""
        , "        expectations = runner.run ()"
        , "        failures = expectations |> List.filterMap (\\expectation -> Test.Runner.getFailureReason expectation |> Maybe.map .description)"
        , "        passed = List.isEmpty failures"
        , "    in"
        , "    { passed = passed, label = labelPath, message = if passed then \"\" else String.join \"; \" failures }"
        ]


{-| Ensure all Elm dependencies are fetched before loading the project.
Uses `elm make` to trigger the Elm compiler's dependency resolver.
No-op if everything is already cached.
-}
ensureDependenciesFetched : Config -> BackendTask FatalError ()
ensureDependenciesFetched config =
    File.rawFile "elm.json"
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\raw ->
                let
                    isPackage =
                        String.contains "\"type\": \"package\"" raw
                            || String.contains "\"type\":\"package\"" raw
                in
                let
                    fetchCmd =
                        if isPackage then
                            Script.exec "elm" [ "make", "--docs", "/tmp/elm-mutation-docs.json" ]

                        else
                            Glob.fromStringWithOptions
                                (let
                                    o =
                                        Glob.defaultOptions
                                 in
                                 { o | include = Glob.OnlyFiles }
                                )
                                "src/**/*.elm"
                                |> BackendTask.andThen
                                    (\files ->
                                        case files of
                                            first :: _ ->
                                                Script.exec "elm" [ "make", first, "--output", "/dev/null" ]

                                            [] ->
                                                BackendTask.succeed ()
                                    )
                in
                -- Non-fatal: if elm make fails (e.g. unrelated compile errors in the project),
                -- continue anyway. The real error will surface during project loading if deps
                -- are truly missing.
                fetchCmd
                    |> BackendTask.toResult
                    |> BackendTask.map (\_ -> ())
            )


{-| Resolve the test file: use --test if provided, otherwise auto-discover.
-}
resolveTestFile : Config -> InterpreterProject -> BackendTask FatalError String
resolveTestFile config project =
    case config.testFile of
        Just explicit ->
            Do.log ("Using test file: " ++ explicit) <| \_ ->
            BackendTask.succeed explicit

        Nothing ->
            case config.mutateFile of
                Just mutateFile ->
                    -- Single-file mode: find tests that import the mutated module
                    Do.allowFatal (File.rawFile mutateFile) <| \mutateSource ->
                    let
                        moduleName =
                            DepGraph.parseModuleName mutateSource
                                |> Maybe.withDefault ""
                    in
                    Do.log ("Auto-discovering test files that import " ++ moduleName ++ "...") <| \_ ->
                    Do.do (findTestFilesImporting moduleName) <| \testFiles ->
                    case testFiles of
                        [ single ] ->
                            Do.log ("Found test file: " ++ single) <| \_ ->
                            BackendTask.succeed single

                        first :: _ ->
                            Do.log ("Found " ++ String.fromInt (List.length testFiles) ++ " test files: " ++ String.join ", " testFiles ++ " (using first)") <| \_ ->
                            BackendTask.succeed first

                        [] ->
                            BackendTask.fail
                                (FatalError.fromString
                                    ("No test files found that import "
                                        ++ moduleName
                                        ++ ". Looked in tests/**/*.elm. Use --test to specify manually."
                                    )
                                )

                Nothing ->
                    -- Multi-file mode: find all test files
                    Do.log "Auto-discovering test files..." <| \_ ->
                    Do.do (findAllTestFiles project) <| \testFiles ->
                    case testFiles of
                        [ single ] ->
                            Do.log ("Found test file: " ++ single) <| \_ ->
                            BackendTask.succeed single

                        first :: _ ->
                            Do.log ("Found " ++ String.fromInt (List.length testFiles) ++ " test files: " ++ String.join ", " testFiles ++ " (using first)") <| \_ ->
                            BackendTask.succeed first

                        [] ->
                            BackendTask.fail
                                (FatalError.fromString
                                    "No test files found in tests/**/*.elm. Use --test to specify manually."
                                )


findTestFilesImporting : String -> BackendTask FatalError (List String)
findTestFilesImporting moduleName =
    Glob.fromStringWithOptions
        (let
            o =
                Glob.defaultOptions
         in
         { o | include = Glob.OnlyFiles }
        )
        "tests/**/*.elm"
        |> BackendTask.andThen
            (\files ->
                files
                    |> List.map
                        (\filePath ->
                            File.rawFile filePath
                                |> BackendTask.allowFatal
                                |> BackendTask.map (\content -> ( filePath, content ))
                        )
                    |> BackendTask.sequence
                    |> BackendTask.map
                        (\pairs ->
                            pairs
                                |> List.filter
                                    (\( _, content ) ->
                                        List.member moduleName (DepGraph.parseImports content)
                                    )
                                |> List.map Tuple.first
                        )
            )


{-| Find all test files by looking for .elm files in tests/ that import Test.
-}
findAllTestFiles : InterpreterProject -> BackendTask FatalError (List String)
findAllTestFiles project =
    Glob.fromStringWithOptions
        (let
            o =
                Glob.defaultOptions
         in
         { o | include = Glob.OnlyFiles }
        )
        "tests/**/*.elm"
        |> BackendTask.andThen
            (\files ->
                files
                    |> List.map
                        (\filePath ->
                            File.rawFile filePath
                                |> BackendTask.allowFatal
                                |> BackendTask.map (\content -> ( filePath, content ))
                        )
                    |> BackendTask.sequence
                    |> BackendTask.map
                        (\pairs ->
                            pairs
                                |> List.filter
                                    (\( _, content ) ->
                                        List.member "Test" (DepGraph.parseImports content)
                                    )
                                |> List.map Tuple.first
                        )
            )


sourceContext : String -> Int -> String
sourceContext source line =
    let
        lines =
            String.lines source

        getLine n =
            lines |> List.drop (n - 1) |> List.head |> Maybe.withDefault ""

        pad n =
            let
                s =
                    String.fromInt n
            in
            String.repeat (4 - String.length s) " " ++ s

        showLine n =
            if n >= 1 && n <= List.length lines then
                let
                    prefix =
                        if n == line then
                            Ansi.Color.fontColor Ansi.Color.red ("  > " ++ pad n ++ " | ")

                        else
                            "    " ++ pad n ++ " | "
                in
                prefix ++ getLine n

            else
                ""
    in
    [ showLine (line - 1), showLine line, showLine (line + 1) ]
        |> List.filter (not << String.isEmpty)
        |> String.join "\n"


kernelPackages : Set.Set String
kernelPackages =
    Set.fromList
        [ "elm/json"
        , "elm/regex"
        , "elm/html"
        , "elm/virtual-dom"
        , "elm/browser"
        , "elm/http"
        , "elm/file"
        , "elm/url"
        ]


patchSource : String -> String
patchSource source =
    if String.contains "runThunk =\n    Elm.Kernel.Test.runThunk" source then
        source
            |> String.replace
                "runThunk =\n    Elm.Kernel.Test.runThunk"
                "runThunk fn =\n    Ok (fn ())"

    else
        source
