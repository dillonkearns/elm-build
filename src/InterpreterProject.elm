module InterpreterProject exposing (EnvMode(..), InterpreterProject, LoadProfile, ModuleGraph, ResolveErrorSummary, benchmarkPackageSummaryCacheCodecs, decodeExposed, decodeFunctionImplementationNoRanges, decodeModuleName, encodeExposed, encodeFunctionImplementationNoRanges, encodeModuleName, eval, evalCachedViaTask, evalSimple, evalWith, evalWithCoverage, evalWithFileOverrides, evalWithSourceOverrides, getDepGraph, getModuleGraph, getPackageEnv, load, loadWith, loadWithPreBuiltGraphs, loadWithProfile, loadWithProfileUserNormalizationFlags, loadWithUserNormalizationFlags, precomputedValuesByModule, precomputedValuesCount, prepareAndEval, prepareAndEvalRaw, prepareAndEvalWithIntercepts, prepareAndEvalWithMemoizedFunctions, prepareAndEvalWithValues, prepareAndEvalWithValuesAndMemoizedFunctions, prepareAndEvalWithYield, prepareAndEvalWithYieldAndMemoizedFunctions, prepareAndEvalWithYieldState, prepareEvalSources, withEnvMode)

{-| Evaluate and cache Elm expressions via the pure Elm interpreter.

Mirrors `ElmProject` structurally but replaces `elm make` + `node` with
`Cache.compute` + `Eval.Module.evalProject`.

-}

import AstWireCodec
import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Glob as Glob
import BackendTask.Time
import Bytes
import Cache exposing (FileOrDirectory)
import Coverage
import DepGraph
import Dict exposing (Dict)
import Elm.Interface exposing (Exposed)
import Elm.Parser
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix as Infix exposing (InfixDirection(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Environment
import Eval.Module
import Eval.Resolver as Resolver
import FNV1a
import FastDict
import FatalError exposing (FatalError)
import FunctionReachability
import Json.Decode as Decode
import Json.Encode
import Lamdera.Wire3
import MemoRuntime
import NormalizationFlags
import Path exposing (Path)
import ProjectRoots
import ProjectSources
import SemanticHash
import Set exposing (Set)
import Syntax exposing (fakeNode)
import Time
import Types
import ValueWireCodec


type alias ModuleGraph =
    { moduleToSource : Dict String String
    , moduleToFile : Dict String File
    , imports : Dict String (Set String)
    }


type alias CachedPackageModuleSummary =
    Eval.Module.CachedModuleSummary


type alias PackageEnvSeed =
    { interfaces : List ( ModuleName, List Exposed )
    , functionsByModule : List ( String, List FunctionImplementation )
    , importsByModule : List ( String, Types.ImportedNames )
    }


type alias LoadProfile =
    { resolveSourceDirectoriesMs : Int
    , loadPackageSourcesMs : Int
    , packageSummaryCacheHit : Int
    , packageSummaryCacheRoundtripOk : Int
    , packageSummaryCacheBytes : Int
    , loadPackageSummaryCacheMs : Int
    , decodePackageSummaryCacheMs : Int
    , validatePackageSummaryCacheMs : Int
    , writePackageSummaryCacheMs : Int
    , globUserSourcesMs : Int
    , readUserSourcesMs : Int
    , readExtraSourcesMs : Int
    , buildGraphMs : Int
    , parsePackageSourcesMs : Int
    , buildPackageSummariesFromParsedMs : Int
    , packageSummaryFunctionsVisited : Int
    , packageSummaryFunctionsRewritten : Int
    , packageSummaryInlineCandidates : Int
    , packageSummaryInlineSuccesses : Int
    , packageSummaryInlineRejectedPattern : Int
    , packageSummaryInlineRejectedArity : Int
    , packageSummaryInlineRejectedSelfCall : Int
    , packageSummaryInlineRejectedBodyTooLarge : Int
    , packageSummaryInlineRejectedUnsafe : Int
    , packageSummaryInlineRejectedUnsafeApplication : Int
    , packageSummaryInlineRejectedUnsafeIf : Int
    , packageSummaryInlineRejectedUnsafeCase : Int
    , packageSummaryInlineRejectedUnsafeLet : Int
    , packageSummaryInlineRejectedUnsafeLambda : Int
    , packageSummaryInlineRejectedUnsafeOther : Int
    , packageSummaryInlineRejectedInternalHelper : Int
    , packageSummaryInlineBodyLt30 : Int
    , packageSummaryInlineBody30To59 : Int
    , packageSummaryInlineBody60Plus : Int
    , packageSummaryInlineShapeLeaf : Int
    , packageSummaryInlineShapeConstructor : Int
    , packageSummaryInlineShapeOperator : Int
    , packageSummaryInlineShapeRecordAccess : Int
    , packageSummaryInlineShapeCollection : Int
    , packageSummaryInlineShapeOther : Int
    , packageSummaryInlinePayoffChanged : Int
    , packageSummaryInlinePayoffChangedShapeLeaf : Int
    , packageSummaryInlinePayoffChangedShapeConstructor : Int
    , packageSummaryInlinePayoffChangedShapeOperator : Int
    , packageSummaryInlinePayoffChangedShapeRecordAccess : Int
    , packageSummaryInlinePayoffChangedShapeCollection : Int
    , packageSummaryInlinePayoffChangedShapeOther : Int
    , packageSummaryInlinePayoffChangedBodyLt30 : Int
    , packageSummaryInlinePayoffChangedBody30To59 : Int
    , packageSummaryInlinePayoffChangedBody60Plus : Int
    , packageSummaryInlinePayoffInline : Int
    , packageSummaryInlinePayoffInlineShapeLeaf : Int
    , packageSummaryInlinePayoffInlineShapeConstructor : Int
    , packageSummaryInlinePayoffInlineShapeOperator : Int
    , packageSummaryInlinePayoffInlineShapeRecordAccess : Int
    , packageSummaryInlinePayoffInlineShapeCollection : Int
    , packageSummaryInlinePayoffInlineShapeOther : Int
    , packageSummaryInlinePayoffInlineBodyLt30 : Int
    , packageSummaryInlinePayoffInlineBody30To59 : Int
    , packageSummaryInlinePayoffInlineBody60Plus : Int
    , packageSummaryInlinePayoffConstantFold : Int
    , packageSummaryInlinePayoffConstantFoldShapeLeaf : Int
    , packageSummaryInlinePayoffConstantFoldShapeConstructor : Int
    , packageSummaryInlinePayoffConstantFoldShapeOperator : Int
    , packageSummaryInlinePayoffConstantFoldShapeRecordAccess : Int
    , packageSummaryInlinePayoffConstantFoldShapeCollection : Int
    , packageSummaryInlinePayoffConstantFoldShapeOther : Int
    , packageSummaryInlinePayoffConstantFoldBodyLt30 : Int
    , packageSummaryInlinePayoffConstantFoldBody30To59 : Int
    , packageSummaryInlinePayoffConstantFoldBody60Plus : Int
    , packageSummaryInlinePayoffPrecomputedRef : Int
    , packageSummaryInlinePayoffPrecomputedRefShapeLeaf : Int
    , packageSummaryInlinePayoffPrecomputedRefShapeConstructor : Int
    , packageSummaryInlinePayoffPrecomputedRefShapeOperator : Int
    , packageSummaryInlinePayoffPrecomputedRefShapeRecordAccess : Int
    , packageSummaryInlinePayoffPrecomputedRefShapeCollection : Int
    , packageSummaryInlinePayoffPrecomputedRefShapeOther : Int
    , packageSummaryInlinePayoffPrecomputedRefBodyLt30 : Int
    , packageSummaryInlinePayoffPrecomputedRefBody30To59 : Int
    , packageSummaryInlinePayoffPrecomputedRefBody60Plus : Int
    , packageSummaryInlineShadowRejectCollection : Int
    , packageSummaryInlineShadowRejectCollectionPayoffChanged : Int
    , packageSummaryInlineShadowRejectCollectionPayoffInline : Int
    , packageSummaryInlineShadowRejectCollectionPayoffPrecomputedRef : Int
    , packageSummaryInlineShadowRejectCollectionFinalShrinks : Int
    , packageSummaryInlineShadowRejectCollectionFinalNonApplication : Int
    , packageSummaryInlineShadowRejectCollectionFinalDirectRootWin : Int
    , packageSummaryInlineShadowRejectCollectionFinalConstructorApplication : Int
    , packageSummaryInlineShadowRejectCollectionNoPayoffNoDirectBenefit : Int
    , packageSummaryInlineShadowRejectGrowth0 : Int
    , packageSummaryInlineShadowRejectGrowth0PayoffChanged : Int
    , packageSummaryInlineShadowRejectGrowth0PayoffInline : Int
    , packageSummaryInlineShadowRejectGrowth0PayoffPrecomputedRef : Int
    , packageSummaryInlineShadowRejectGrowth0FinalShrinks : Int
    , packageSummaryInlineShadowRejectGrowth0FinalNonApplication : Int
    , packageSummaryInlineShadowRejectGrowth0FinalDirectRootWin : Int
    , packageSummaryInlineShadowRejectGrowth0FinalConstructorApplication : Int
    , packageSummaryInlineShadowRejectGrowth0NoPayoffNoDirectBenefit : Int
    , packageSummaryInlineShadowRejectGrowth1 : Int
    , packageSummaryInlineShadowRejectGrowth1PayoffChanged : Int
    , packageSummaryInlineShadowRejectGrowth1PayoffInline : Int
    , packageSummaryInlineShadowRejectGrowth1PayoffPrecomputedRef : Int
    , packageSummaryInlineShadowRejectGrowth1FinalShrinks : Int
    , packageSummaryInlineShadowRejectGrowth1FinalNonApplication : Int
    , packageSummaryInlineShadowRejectGrowth1FinalDirectRootWin : Int
    , packageSummaryInlineShadowRejectGrowth1FinalConstructorApplication : Int
    , packageSummaryInlineShadowRejectGrowth1NoPayoffNoDirectBenefit : Int
    , packageSummaryListFusionChanges : Int
    , packageSummaryListFusionPipelineNormalizations : Int
    , packageSummaryListFusionHeadFlattenRewrites : Int
    , packageSummaryListFusionRuleRewrites : Int
    , packageSummaryPrecomputedRefSubstitutions : Int
    , packageSummaryConstantFolds : Int
    , packageSummaryRejectSamples : List String
    , buildPackageEnvFromSummariesMs : Int
    , buildPackageEnvMs : Int
    , packageResolvedErrorsCount : Int
    , packageResolvedBodiesCount : Int
    , packageResolvedGlobalsCount : Int
    , packageResolvedErrorSummary : ResolveErrorSummary
    , resolvedErrorsCount : Int
    , resolvedBodiesCount : Int
    , resolvedGlobalsCount : Int
    , resolvedErrorSummary : ResolveErrorSummary
    , buildBaseUserEnvMs : Int
    , userNormModulesPlanned : Int
    , userNormTargetFunctions : Int
    , userNormCacheHitModules : Int
    , userNormCacheMissModules : Int
    , userNormCacheExtendedModules : Int
    , userNormRewrittenFunctions : Int
    , userNormPrecomputedValues : Int
    , userNormDependencySummaryStats : Eval.Module.DependencySummaryStats
    , buildSemanticIndexMs : Int
    , cacheInputsMs : Int
    }


type alias DependencySummaryStats =
    { functionsVisited : Int
    , functionsRewritten : Int
    , inlineCandidates : Int
    , inlineSuccesses : Int
    , inlineRejectedPattern : Int
    , inlineRejectedArity : Int
    , inlineRejectedSelfCall : Int
    , inlineRejectedBodyTooLarge : Int
    , inlineRejectedUnsafe : Int
    , inlineRejectedUnsafeApplication : Int
    , inlineRejectedUnsafeIf : Int
    , inlineRejectedUnsafeCase : Int
    , inlineRejectedUnsafeLet : Int
    , inlineRejectedUnsafeLambda : Int
    , inlineRejectedUnsafeOther : Int
    , inlineRejectedInternalHelper : Int
    , inlineBodyLt30 : Int
    , inlineBody30To59 : Int
    , inlineBody60Plus : Int
    , inlineShapeLeaf : Int
    , inlineShapeConstructor : Int
    , inlineShapeOperator : Int
    , inlineShapeRecordAccess : Int
    , inlineShapeCollection : Int
    , inlineShapeOther : Int
    , inlinePayoffChanged : Int
    , inlinePayoffChangedShapeLeaf : Int
    , inlinePayoffChangedShapeConstructor : Int
    , inlinePayoffChangedShapeOperator : Int
    , inlinePayoffChangedShapeRecordAccess : Int
    , inlinePayoffChangedShapeCollection : Int
    , inlinePayoffChangedShapeOther : Int
    , inlinePayoffChangedBodyLt30 : Int
    , inlinePayoffChangedBody30To59 : Int
    , inlinePayoffChangedBody60Plus : Int
    , inlinePayoffInline : Int
    , inlinePayoffInlineShapeLeaf : Int
    , inlinePayoffInlineShapeConstructor : Int
    , inlinePayoffInlineShapeOperator : Int
    , inlinePayoffInlineShapeRecordAccess : Int
    , inlinePayoffInlineShapeCollection : Int
    , inlinePayoffInlineShapeOther : Int
    , inlinePayoffInlineBodyLt30 : Int
    , inlinePayoffInlineBody30To59 : Int
    , inlinePayoffInlineBody60Plus : Int
    , inlinePayoffConstantFold : Int
    , inlinePayoffConstantFoldShapeLeaf : Int
    , inlinePayoffConstantFoldShapeConstructor : Int
    , inlinePayoffConstantFoldShapeOperator : Int
    , inlinePayoffConstantFoldShapeRecordAccess : Int
    , inlinePayoffConstantFoldShapeCollection : Int
    , inlinePayoffConstantFoldShapeOther : Int
    , inlinePayoffConstantFoldBodyLt30 : Int
    , inlinePayoffConstantFoldBody30To59 : Int
    , inlinePayoffConstantFoldBody60Plus : Int
    , inlinePayoffPrecomputedRef : Int
    , inlinePayoffPrecomputedRefShapeLeaf : Int
    , inlinePayoffPrecomputedRefShapeConstructor : Int
    , inlinePayoffPrecomputedRefShapeOperator : Int
    , inlinePayoffPrecomputedRefShapeRecordAccess : Int
    , inlinePayoffPrecomputedRefShapeCollection : Int
    , inlinePayoffPrecomputedRefShapeOther : Int
    , inlinePayoffPrecomputedRefBodyLt30 : Int
    , inlinePayoffPrecomputedRefBody30To59 : Int
    , inlinePayoffPrecomputedRefBody60Plus : Int
    , inlineShadowRejectCollection : Int
    , inlineShadowRejectCollectionPayoffChanged : Int
    , inlineShadowRejectCollectionPayoffInline : Int
    , inlineShadowRejectCollectionPayoffPrecomputedRef : Int
    , inlineShadowRejectCollectionFinalShrinks : Int
    , inlineShadowRejectCollectionFinalNonApplication : Int
    , inlineShadowRejectCollectionFinalDirectRootWin : Int
    , inlineShadowRejectCollectionFinalConstructorApplication : Int
    , inlineShadowRejectCollectionNoPayoffNoDirectBenefit : Int
    , inlineShadowRejectGrowth0 : Int
    , inlineShadowRejectGrowth0PayoffChanged : Int
    , inlineShadowRejectGrowth0PayoffInline : Int
    , inlineShadowRejectGrowth0PayoffPrecomputedRef : Int
    , inlineShadowRejectGrowth0FinalShrinks : Int
    , inlineShadowRejectGrowth0FinalNonApplication : Int
    , inlineShadowRejectGrowth0FinalDirectRootWin : Int
    , inlineShadowRejectGrowth0FinalConstructorApplication : Int
    , inlineShadowRejectGrowth0NoPayoffNoDirectBenefit : Int
    , inlineShadowRejectGrowth1 : Int
    , inlineShadowRejectGrowth1PayoffChanged : Int
    , inlineShadowRejectGrowth1PayoffInline : Int
    , inlineShadowRejectGrowth1PayoffPrecomputedRef : Int
    , inlineShadowRejectGrowth1FinalShrinks : Int
    , inlineShadowRejectGrowth1FinalNonApplication : Int
    , inlineShadowRejectGrowth1FinalDirectRootWin : Int
    , inlineShadowRejectGrowth1FinalConstructorApplication : Int
    , inlineShadowRejectGrowth1NoPayoffNoDirectBenefit : Int
    , listFusionChanges : Int
    , listFusionPipelineNormalizations : Int
    , listFusionHeadFlattenRewrites : Int
    , listFusionRuleRewrites : Int
    , precomputedRefSubstitutions : Int
    , constantFolds : Int
    , rejectSamples : List String
    }


emptyPackageSummaryStats : DependencySummaryStats
emptyPackageSummaryStats =
    { functionsVisited = 0
    , functionsRewritten = 0
    , inlineCandidates = 0
    , inlineSuccesses = 0
    , inlineRejectedPattern = 0
    , inlineRejectedArity = 0
    , inlineRejectedSelfCall = 0
    , inlineRejectedBodyTooLarge = 0
    , inlineRejectedUnsafe = 0
    , inlineRejectedUnsafeApplication = 0
    , inlineRejectedUnsafeIf = 0
    , inlineRejectedUnsafeCase = 0
    , inlineRejectedUnsafeLet = 0
    , inlineRejectedUnsafeLambda = 0
    , inlineRejectedUnsafeOther = 0
    , inlineRejectedInternalHelper = 0
    , inlineBodyLt30 = 0
    , inlineBody30To59 = 0
    , inlineBody60Plus = 0
    , inlineShapeLeaf = 0
    , inlineShapeConstructor = 0
    , inlineShapeOperator = 0
    , inlineShapeRecordAccess = 0
    , inlineShapeCollection = 0
    , inlineShapeOther = 0
    , inlinePayoffChanged = 0
    , inlinePayoffChangedShapeLeaf = 0
    , inlinePayoffChangedShapeConstructor = 0
    , inlinePayoffChangedShapeOperator = 0
    , inlinePayoffChangedShapeRecordAccess = 0
    , inlinePayoffChangedShapeCollection = 0
    , inlinePayoffChangedShapeOther = 0
    , inlinePayoffChangedBodyLt30 = 0
    , inlinePayoffChangedBody30To59 = 0
    , inlinePayoffChangedBody60Plus = 0
    , inlinePayoffInline = 0
    , inlinePayoffInlineShapeLeaf = 0
    , inlinePayoffInlineShapeConstructor = 0
    , inlinePayoffInlineShapeOperator = 0
    , inlinePayoffInlineShapeRecordAccess = 0
    , inlinePayoffInlineShapeCollection = 0
    , inlinePayoffInlineShapeOther = 0
    , inlinePayoffInlineBodyLt30 = 0
    , inlinePayoffInlineBody30To59 = 0
    , inlinePayoffInlineBody60Plus = 0
    , inlinePayoffConstantFold = 0
    , inlinePayoffConstantFoldShapeLeaf = 0
    , inlinePayoffConstantFoldShapeConstructor = 0
    , inlinePayoffConstantFoldShapeOperator = 0
    , inlinePayoffConstantFoldShapeRecordAccess = 0
    , inlinePayoffConstantFoldShapeCollection = 0
    , inlinePayoffConstantFoldShapeOther = 0
    , inlinePayoffConstantFoldBodyLt30 = 0
    , inlinePayoffConstantFoldBody30To59 = 0
    , inlinePayoffConstantFoldBody60Plus = 0
    , inlinePayoffPrecomputedRef = 0
    , inlinePayoffPrecomputedRefShapeLeaf = 0
    , inlinePayoffPrecomputedRefShapeConstructor = 0
    , inlinePayoffPrecomputedRefShapeOperator = 0
    , inlinePayoffPrecomputedRefShapeRecordAccess = 0
    , inlinePayoffPrecomputedRefShapeCollection = 0
    , inlinePayoffPrecomputedRefShapeOther = 0
    , inlinePayoffPrecomputedRefBodyLt30 = 0
    , inlinePayoffPrecomputedRefBody30To59 = 0
    , inlinePayoffPrecomputedRefBody60Plus = 0
    , inlineShadowRejectCollection = 0
    , inlineShadowRejectCollectionPayoffChanged = 0
    , inlineShadowRejectCollectionPayoffInline = 0
    , inlineShadowRejectCollectionPayoffPrecomputedRef = 0
    , inlineShadowRejectCollectionFinalShrinks = 0
    , inlineShadowRejectCollectionFinalNonApplication = 0
    , inlineShadowRejectCollectionFinalDirectRootWin = 0
    , inlineShadowRejectCollectionFinalConstructorApplication = 0
    , inlineShadowRejectCollectionNoPayoffNoDirectBenefit = 0
    , inlineShadowRejectGrowth0 = 0
    , inlineShadowRejectGrowth0PayoffChanged = 0
    , inlineShadowRejectGrowth0PayoffInline = 0
    , inlineShadowRejectGrowth0PayoffPrecomputedRef = 0
    , inlineShadowRejectGrowth0FinalShrinks = 0
    , inlineShadowRejectGrowth0FinalNonApplication = 0
    , inlineShadowRejectGrowth0FinalDirectRootWin = 0
    , inlineShadowRejectGrowth0FinalConstructorApplication = 0
    , inlineShadowRejectGrowth0NoPayoffNoDirectBenefit = 0
    , inlineShadowRejectGrowth1 = 0
    , inlineShadowRejectGrowth1PayoffChanged = 0
    , inlineShadowRejectGrowth1PayoffInline = 0
    , inlineShadowRejectGrowth1PayoffPrecomputedRef = 0
    , inlineShadowRejectGrowth1FinalShrinks = 0
    , inlineShadowRejectGrowth1FinalNonApplication = 0
    , inlineShadowRejectGrowth1FinalDirectRootWin = 0
    , inlineShadowRejectGrowth1FinalConstructorApplication = 0
    , inlineShadowRejectGrowth1NoPayoffNoDirectBenefit = 0
    , listFusionChanges = 0
    , listFusionPipelineNormalizations = 0
    , listFusionHeadFlattenRewrites = 0
    , listFusionRuleRewrites = 0
    , precomputedRefSubstitutions = 0
    , constantFolds = 0
    , rejectSamples = []
    }


normalizeSummariesWithStats :
    List CachedPackageModuleSummary
    -> { summaries : List CachedPackageModuleSummary, stats : DependencySummaryStats }
normalizeSummariesWithStats summaries =
    { summaries = Eval.Module.normalizeSummaries summaries
    , stats = emptyPackageSummaryStats
    }


normalizeOneModuleInEnvSelected :
    Maybe (Set String)
    -> ModuleName
    -> Eval.Module.ProjectEnv
    -> ( Eval.Module.ProjectEnv, FastDict.Dict String FunctionImplementation, FastDict.Dict String Types.Value )
normalizeOneModuleInEnvSelected targetFunctions moduleName envBeforeNorm =
    let
        ( updatedEnv, normalizedFns ) =
            Eval.Module.normalizeOneModuleInEnv moduleName envBeforeNorm

        filterSelected dict =
            case targetFunctions of
                Nothing ->
                    dict

                Just selected ->
                    dict
                        |> FastDict.toList
                        |> List.filter (\( name, _ ) -> Set.member name selected)
                        |> FastDict.fromList
    in
    ( updatedEnv
    , filterSelected normalizedFns
    , Eval.Module.getModulePrecomputedValues moduleName updatedEnv
        |> filterSelected
    )


type alias Timed a =
    { value : a
    , ms : Int
    }


type alias ResolveErrorSummary =
    { totalCount : Int
    , unknownNameCount : Int
    , unknownOperatorCount : Int
    , unsupportedExpressionCount : Int
    , invalidRecordUpdateTargetCount : Int
    , unexpectedTupleArityCount : Int
    , samples : List String
    }


emptyResolveErrorSummary : ResolveErrorSummary
emptyResolveErrorSummary =
    { totalCount = 0
    , unknownNameCount = 0
    , unknownOperatorCount = 0
    , unsupportedExpressionCount = 0
    , invalidRecordUpdateTargetCount = 0
    , unexpectedTupleArityCount = 0
    , samples = []
    }


resolveErrorSampleLimit : Int
resolveErrorSampleLimit =
    12


summarizeResolveErrors : List Eval.Module.ResolveErrorEntry -> ResolveErrorSummary
summarizeResolveErrors entries =
    entries
        |> List.foldl
            (\entry acc ->
                let
                    counted =
                        case entry.error of
                            Resolver.UnknownName _ ->
                                { acc | totalCount = acc.totalCount + 1, unknownNameCount = acc.unknownNameCount + 1 }

                            Resolver.UnknownOperator _ ->
                                { acc | totalCount = acc.totalCount + 1, unknownOperatorCount = acc.unknownOperatorCount + 1 }

                            Resolver.UnsupportedExpression _ ->
                                { acc | totalCount = acc.totalCount + 1, unsupportedExpressionCount = acc.unsupportedExpressionCount + 1 }

                            Resolver.InvalidRecordUpdateTarget _ ->
                                { acc | totalCount = acc.totalCount + 1, invalidRecordUpdateTargetCount = acc.invalidRecordUpdateTargetCount + 1 }

                            Resolver.UnexpectedTupleArity _ ->
                                { acc | totalCount = acc.totalCount + 1, unexpectedTupleArityCount = acc.unexpectedTupleArityCount + 1 }
                in
                { counted | samples = addResolveErrorSample (renderResolveErrorSample entry) counted.samples }
            )
            emptyResolveErrorSummary


addResolveErrorSample : String -> List String -> List String
addResolveErrorSample sample samples =
    if List.member sample samples || List.length samples >= resolveErrorSampleLimit then
        samples

    else
        samples ++ [ sample ]


renderResolveErrorSample : Eval.Module.ResolveErrorEntry -> String
renderResolveErrorSample entry =
    qualifiedNameString entry.moduleName entry.name
        ++ " -> "
        ++ resolveErrorToString entry.error


qualifiedNameString : ModuleName -> String -> String
qualifiedNameString moduleName name =
    if List.isEmpty moduleName then
        name

    else
        String.join "." moduleName ++ "." ++ name


resolveErrorToString : Resolver.ResolveError -> String
resolveErrorToString err =
    case err of
        Resolver.UnknownName { moduleName, name } ->
            "UnknownName " ++ qualifiedNameString moduleName name

        Resolver.UnknownOperator op ->
            "UnknownOperator " ++ op

        Resolver.UnsupportedExpression msg ->
            "UnsupportedExpression " ++ msg

        Resolver.InvalidRecordUpdateTarget name ->
            "InvalidRecordUpdateTarget " ++ name

        Resolver.UnexpectedTupleArity n ->
            "UnexpectedTupleArity " ++ String.fromInt n


stageMs : Time.Posix -> Time.Posix -> Int
stageMs start finish =
    Time.posixToMillis finish - Time.posixToMillis start


withTiming : BackendTask FatalError a -> BackendTask FatalError (Timed a)
withTiming work =
    Do.do BackendTask.Time.now <|
        \start ->
            Do.do work <|
                \value ->
                    Do.do BackendTask.Time.now <|
                        \finish ->
                            BackendTask.succeed
                                { value = value
                                , ms = stageMs start finish
                                }


benchmarkThunk : (() -> a) -> BackendTask FatalError (Timed a)
benchmarkThunk thunk =
    Do.do BackendTask.Time.now <|
        \start ->
            let
                value =
                    thunk ()
            in
            Do.do BackendTask.Time.now <|
                \finish ->
                    BackendTask.succeed
                        { value = value
                        , ms = stageMs start finish
                        }


readFilesTask : List String -> BackendTask FatalError (List String)
readFilesTask paths =
    BackendTask.Custom.run "readFiles"
        (Json.Encode.list Json.Encode.string paths)
        (Decode.list Decode.string)
        |> BackendTask.allowFatal


repeatBinaryEncode : Int -> List CachedPackageModuleSummary -> Int -> Int
repeatBinaryEncode iterations summaries acc =
    if iterations <= 0 then
        acc

    else
        repeatBinaryEncode
            (iterations - 1)
            summaries
            (acc + Bytes.width (encodePackageSummaryCache summaries))


repeatBinaryDecode : Int -> Bytes.Bytes -> Int -> Int
repeatBinaryDecode iterations bytes acc =
    if iterations <= 0 then
        acc

    else
        case decodePackageSummaryCache bytes of
            Just summaries ->
                repeatBinaryDecode (iterations - 1) bytes (acc + List.length summaries)

            Nothing ->
                -1


repeatEnvSeedBinaryEncode : Int -> PackageEnvSeed -> Int -> Int
repeatEnvSeedBinaryEncode iterations envSeed acc =
    if iterations <= 0 then
        acc

    else
        repeatEnvSeedBinaryEncode
            (iterations - 1)
            envSeed
            (acc + Bytes.width (encodePackageEnvSeed envSeed))


repeatEnvSeedBinaryDecode : Int -> Bytes.Bytes -> Int -> Int
repeatEnvSeedBinaryDecode iterations bytes acc =
    if iterations <= 0 then
        acc

    else
        case decodePackageEnvSeed bytes of
            Just envSeed ->
                repeatEnvSeedBinaryDecode
                    (iterations - 1)
                    bytes
                    (acc
                        + List.length envSeed.interfaces
                        + List.length envSeed.functionsByModule
                        + List.length envSeed.importsByModule
                    )

            Nothing ->
                -1


repeatShardedBinaryEncode : Int -> List CachedPackageModuleSummary -> Int -> Int
repeatShardedBinaryEncode iterations summaries acc =
    if iterations <= 0 then
        acc

    else
        let
            shardBytes =
                encodePackageSummaryCacheSharded summaries
        in
        repeatShardedBinaryEncode
            (iterations - 1)
            summaries
            (acc + List.foldl (\bytes total -> total + Bytes.width bytes) 0 shardBytes)


repeatShardedBinaryDecode : Int -> List Bytes.Bytes -> Int -> Int
repeatShardedBinaryDecode iterations shardBytes acc =
    if iterations <= 0 then
        acc

    else
        case decodePackageSummaryCacheSharded shardBytes of
            Just summaries ->
                repeatShardedBinaryDecode (iterations - 1) shardBytes (acc + List.length summaries)

            Nothing ->
                -1


repeatJsonEncode : Int -> List CachedPackageModuleSummary -> Int -> Int
repeatJsonEncode iterations summaries acc =
    if iterations <= 0 then
        acc

    else
        let
            jsonString =
                encodePackageSummaryCacheJson summaries
        in
        repeatJsonEncode (iterations - 1) summaries (acc + String.length jsonString)


repeatJsonDecode : Int -> String -> Int -> Int
repeatJsonDecode iterations jsonString acc =
    if iterations <= 0 then
        acc

    else
        case decodePackageSummaryCacheJson jsonString of
            Just summaries ->
                repeatJsonDecode (iterations - 1) jsonString (acc + List.length summaries)

            Nothing ->
                -1


packageSummaryCacheVersion : String
packageSummaryCacheVersion =
    "v22"


packageSummaryCacheBlobPath : String -> String -> String
packageSummaryCacheBlobPath cacheDir cacheKey =
    cacheDir ++ "/package-module-summaries-" ++ packageSummaryCacheVersion ++ "-" ++ cacheKey ++ ".blob"


packageSummaryCacheKey : List String -> String
packageSummaryCacheKey allPackageSources =
    allPackageSources
        |> List.map (FNV1a.hash >> String.fromInt)
        |> String.join "|"
        |> FNV1a.hash
        |> String.fromInt


{-| Cache version for the user-code normalization blob. Bump whenever
the `FunctionImplementation` wire format or the normalization rewrite shape
changes in a way that would make old blobs incorrect.
-}
userNormCacheVersion : String
userNormCacheVersion =
    "v5"


{-| Disk path for the combined user-normalization cache blob.

Key includes a combined hash over every user file's content and the package
environment fingerprint. Any user or package change invalidates the whole
blob and triggers a full re-normalize — coarser than per-file caching but
dramatically faster on the common "nothing changed" warm path (one binary
read + decode instead of dozens of stat/read round-trips).

-}
userNormCacheBlobPath : String -> String -> String -> String
userNormCacheBlobPath cacheDir packageKey userKey =
    cacheDir ++ "/user-normalized-" ++ userNormCacheVersion ++ "-" ++ packageKey ++ "-" ++ userKey ++ ".blob"


{-| Combined hash over every user file's source content, independent of
iteration order. Any byte change in any user file invalidates the blob.
-}
userNormBundleKey : List String -> String
userNormBundleKey userFileContents =
    userFileContents
        |> List.map (FNV1a.hash >> String.fromInt)
        |> List.sort
        |> String.join "|"
        |> FNV1a.hash
        |> String.fromInt


userNormalizationFlagsKey : NormalizationFlags.NormalizationFlags -> String
userNormalizationFlagsKey flags =
    [ if flags.foldConstantApplications then
        "1"

      else
        "0"
    , if flags.inlinePrecomputedRefs then
        "1"

      else
        "0"
    , if flags.inlineFunctions then
        "1"

      else
        "0"
    , String.fromInt flags.inlineFunctionMaxSize
    , if flags.fuseListMaps then
        "1"

      else
        "0"
    , if flags.runFixpoint then
        "1"

      else
        "0"
    , if flags.runListFusion then
        "1"

      else
        "0"
    , String.fromInt flags.fixpointPasses
    , String.fromInt flags.tryNormalizeMaxSteps
    ]
        |> String.join "|"
        |> FNV1a.hash
        |> String.fromInt


{-| Entry in the combined user-normalization blob: one per module, carrying
the module name, the list of normalized function implementations, and any
precomputed constant values (zero-arg functions evaluated to concrete Values
that survived `isLosslessValue`).
-}
type alias UserNormCacheEntry =
    { moduleName : List String
    , functions : List FunctionImplementation
    , precomputedValues : List ( String, Types.Value )
    , attemptedFunctions : List String
    }


type alias UserNormModulePlan =
    { file : File
    , targetFunctions : Maybe (Set String)
    }


type alias UserNormStats =
    { modulesPlanned : Int
    , targetFunctions : Int
    , cacheHitModules : Int
    , cacheMissModules : Int
    , cacheExtendedModules : Int
    , rewrittenFunctions : Int
    , precomputedValues : Int
    , dependencySummaryStats : Eval.Module.DependencySummaryStats
    }


type alias UserNormBuildResult =
    { env : Eval.Module.ProjectEnv
    , stats : UserNormStats
    }


encodeUserNormBundle : List UserNormCacheEntry -> Bytes.Bytes
encodeUserNormBundle entries =
    entries
        |> Lamdera.Wire3.encodeList encodeUserNormCacheEntry
        |> Lamdera.Wire3.bytesEncode


decodeUserNormBundle : Bytes.Bytes -> Maybe (List UserNormCacheEntry)
decodeUserNormBundle bytes =
    bytes
        |> Lamdera.Wire3.bytesDecode (Lamdera.Wire3.decodeList decodeUserNormCacheEntry)


encodeUserNormCacheEntry : UserNormCacheEntry -> Lamdera.Wire3.Encoder
encodeUserNormCacheEntry entry =
    -- Filter precomputed values to only those that round-trip through Wire3.
    -- `Int Infinity` (from `round (1 / 0)`), non-finite Floats, `JsonValue`,
    -- `PartiallyApplied`, etc. are held in the in-memory precomputed cache
    -- for runtime speedups but must not hit the on-disk blob — their encoded
    -- bytes don't decode back to the same value (or at all), which silently
    -- corrupts subsequent warm-run reads.
    let
        serializable : List ( String, Types.Value )
        serializable =
            List.filter
                (\( _, value ) -> Eval.Module.isLosslessValue value)
                entry.precomputedValues
    in
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ encodeModuleName entry.moduleName
        , Lamdera.Wire3.encodeList encodeFunctionImplementationNoRanges entry.functions
        , Lamdera.Wire3.encodeList
            (\( name, value ) ->
                Lamdera.Wire3.encodeSequenceWithoutLength
                    [ Lamdera.Wire3.encodeString name
                    , ValueWireCodec.encodeValue value
                    ]
            )
            serializable
        , Lamdera.Wire3.encodeList Lamdera.Wire3.encodeString entry.attemptedFunctions
        ]


decodeUserNormCacheEntry : Lamdera.Wire3.Decoder UserNormCacheEntry
decodeUserNormCacheEntry =
    Lamdera.Wire3.succeedDecode
        (\moduleName functions precomputedValues attemptedFunctions ->
            { moduleName = moduleName
            , functions = functions
            , precomputedValues = precomputedValues
            , attemptedFunctions = attemptedFunctions
            }
        )
        |> Lamdera.Wire3.andMapDecode decodeModuleName
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeFunctionImplementationNoRanges)
        |> Lamdera.Wire3.andMapDecode
            (Lamdera.Wire3.decodeList
                (Lamdera.Wire3.succeedDecode Tuple.pair
                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                    |> Lamdera.Wire3.andMapDecode ValueWireCodec.decodeValue
                )
            )
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList Lamdera.Wire3.decodeString)


benchmarkPackageSummaryCacheCodecs :
    { projectDir : Path
    , skipPackages : Set String
    , patchSource : String -> String
    , patchUserSource : String -> String -> String
    , extraSourceFiles : List String
    , extraReachableImports : List String
    , sourceDirectories : Maybe (List String)
    , packageParseCacheDir : String
    , iterations : Int
    }
    ->
        BackendTask
            FatalError
            { iterations : Int
            , summaryCount : Int
            , binaryBytes : Int
            , metadataOnlyBinaryBytes : Int
            , topFunctionPayloadModules : List { moduleKey : String, functionCount : Int, functionBytes : Int }
            , envSeedBinaryBytes : Int
            , shardedBinaryBytes : Int
            , jsonChars : Int
            , seedCacheHit : Int
            , seedParsePackageSourcesMs : Int
            , seedBuildPackageSummariesFromParsedMs : Int
            , seedDecodePackageSummaryCacheMs : Int
            , binaryEncodeMs : Int
            , binaryDecodeMs : Int
            , metadataOnlyBinaryEncodeMs : Int
            , metadataOnlyBinaryDecodeMs : Int
            , envSeedBinaryEncodeMs : Int
            , envSeedBinaryDecodeMs : Int
            , shardedBinaryEncodeMs : Int
            , shardedBinaryDecodeMs : Int
            , jsonEncodeMs : Int
            , jsonDecodeMs : Int
            }
benchmarkPackageSummaryCacheCodecs config =
    Do.do
        (loadWithProfile
            { projectDir = config.projectDir
            , skipPackages = config.skipPackages
            , patchSource = config.patchSource
            , patchUserSource = \_ source -> source
            , extraSourceFiles = config.extraSourceFiles
            , extraReachableImports = config.extraReachableImports
            , sourceDirectories = config.sourceDirectories
            , normalizationRoots = Nothing
            , packageParseCacheDir = Just config.packageParseCacheDir
            , preBuiltDepGraph = Nothing
            , preBuiltModuleGraph = Nothing
            }
        )
    <|
        \seedLoad ->
            Do.do
                (Glob.fromStringWithOptions
                    (let
                        options : Glob.Options
                        options =
                            Glob.defaultOptions
                     in
                     { options | include = Glob.OnlyFiles }
                    )
                    (config.packageParseCacheDir ++ "/package-module-summaries-" ++ packageSummaryCacheVersion ++ "-*.blob")
                )
            <|
                \cacheCandidates ->
                    case List.head (List.sort cacheCandidates) of
                        Nothing ->
                            BackendTask.fail (FatalError.fromString "Failed to find package summary cache blob for codec benchmark")

                        Just cachePath ->
                            Do.do (File.binaryFile cachePath |> BackendTask.allowFatal) <|
                                \cacheBytes ->
                                    case decodePackageSummaryCache cacheBytes of
                                        Nothing ->
                                            BackendTask.fail (FatalError.fromString "Failed to decode package summary cache blob for codec benchmark")

                                        Just summaries ->
                                            let
                                                metadataOnlySummaries =
                                                    summaries
                                                        |> List.map (\summary -> { summary | functions = [] })

                                                envSeed =
                                                    packageEnvSeedFromSummaries summaries

                                                envSeedBytes =
                                                    encodePackageEnvSeed envSeed

                                                metadataOnlyBytes =
                                                    encodePackageSummaryCache metadataOnlySummaries

                                                topFunctionPayloadModules =
                                                    summaries
                                                        |> List.map
                                                            (\summary ->
                                                                { moduleKey = Environment.moduleKey summary.moduleName
                                                                , functionCount = List.length summary.functions
                                                                , functionBytes =
                                                                    summary.functions
                                                                        |> Lamdera.Wire3.encodeList encodeFunctionImplementationNoRanges
                                                                        |> Lamdera.Wire3.bytesEncode
                                                                        |> Bytes.width
                                                                }
                                                            )
                                                        |> List.sortBy (.functionBytes >> negate)
                                                        |> List.take 12

                                                jsonString =
                                                    encodePackageSummaryCacheJson summaries

                                                shardedBytes =
                                                    encodePackageSummaryCacheSharded summaries
                                            in
                                            Do.do (benchmarkThunk (\_ -> repeatBinaryEncode config.iterations summaries 0)) <|
                                                \binaryEncodeTimed ->
                                                    Do.do (benchmarkThunk (\_ -> repeatBinaryDecode config.iterations cacheBytes 0)) <|
                                                        \binaryDecodeTimed ->
                                                            Do.do (benchmarkThunk (\_ -> repeatBinaryEncode config.iterations metadataOnlySummaries 0)) <|
                                                                \metadataOnlyBinaryEncodeTimed ->
                                                                    Do.do (benchmarkThunk (\_ -> repeatBinaryDecode config.iterations metadataOnlyBytes 0)) <|
                                                                        \metadataOnlyBinaryDecodeTimed ->
                                                                            Do.do (benchmarkThunk (\_ -> repeatEnvSeedBinaryEncode config.iterations envSeed 0)) <|
                                                                                \envSeedBinaryEncodeTimed ->
                                                                                    Do.do (benchmarkThunk (\_ -> repeatEnvSeedBinaryDecode config.iterations envSeedBytes 0)) <|
                                                                                        \envSeedBinaryDecodeTimed ->
                                                                                            Do.do (benchmarkThunk (\_ -> repeatShardedBinaryEncode config.iterations summaries 0)) <|
                                                                                                \shardedBinaryEncodeTimed ->
                                                                                                    Do.do (benchmarkThunk (\_ -> repeatShardedBinaryDecode config.iterations shardedBytes 0)) <|
                                                                                                        \shardedBinaryDecodeTimed ->
                                                                                                            Do.do (benchmarkThunk (\_ -> repeatJsonEncode config.iterations summaries 0)) <|
                                                                                                                \jsonEncodeTimed ->
                                                                                                                    Do.do (benchmarkThunk (\_ -> repeatJsonDecode config.iterations jsonString 0)) <|
                                                                                                                        \jsonDecodeTimed ->
                                                                                                                            BackendTask.succeed
                                                                                                                                { iterations = config.iterations
                                                                                                                                , summaryCount = List.length summaries
                                                                                                                                , binaryBytes = Bytes.width cacheBytes
                                                                                                                                , metadataOnlyBinaryBytes = Bytes.width metadataOnlyBytes
                                                                                                                                , topFunctionPayloadModules = topFunctionPayloadModules
                                                                                                                                , envSeedBinaryBytes = Bytes.width envSeedBytes
                                                                                                                                , shardedBinaryBytes = List.foldl (\bytes total -> total + Bytes.width bytes) 0 shardedBytes
                                                                                                                                , jsonChars = String.length jsonString
                                                                                                                                , seedCacheHit = seedLoad.profile.packageSummaryCacheHit
                                                                                                                                , seedParsePackageSourcesMs = seedLoad.profile.parsePackageSourcesMs
                                                                                                                                , seedBuildPackageSummariesFromParsedMs = seedLoad.profile.buildPackageSummariesFromParsedMs
                                                                                                                                , seedDecodePackageSummaryCacheMs = seedLoad.profile.decodePackageSummaryCacheMs
                                                                                                                                , binaryEncodeMs = binaryEncodeTimed.ms
                                                                                                                                , binaryDecodeMs = binaryDecodeTimed.ms
                                                                                                                                , metadataOnlyBinaryEncodeMs = metadataOnlyBinaryEncodeTimed.ms
                                                                                                                                , metadataOnlyBinaryDecodeMs = metadataOnlyBinaryDecodeTimed.ms
                                                                                                                                , envSeedBinaryEncodeMs = envSeedBinaryEncodeTimed.ms
                                                                                                                                , envSeedBinaryDecodeMs = envSeedBinaryDecodeTimed.ms
                                                                                                                                , shardedBinaryEncodeMs = shardedBinaryEncodeTimed.ms
                                                                                                                                , shardedBinaryDecodeMs = shardedBinaryDecodeTimed.ms
                                                                                                                                , jsonEncodeMs = jsonEncodeTimed.ms
                                                                                                                                , jsonDecodeMs = jsonDecodeTimed.ms
                                                                                                                                }


encodePackageSummaryCache : List CachedPackageModuleSummary -> Bytes.Bytes
encodePackageSummaryCache summaries =
    summaries
        |> Lamdera.Wire3.encodeList encodeCachedPackageModuleSummary
        |> Lamdera.Wire3.bytesEncode


decodePackageSummaryCache : Bytes.Bytes -> Maybe (List CachedPackageModuleSummary)
decodePackageSummaryCache bytes =
    bytes
        |> Lamdera.Wire3.bytesDecode (Lamdera.Wire3.decodeList decodeCachedPackageModuleSummary)


encodePackageSummaryCacheSharded : List CachedPackageModuleSummary -> List Bytes.Bytes
encodePackageSummaryCacheSharded summaries =
    List.map
        (\summary ->
            summary
                |> encodeCachedPackageModuleSummary
                |> Lamdera.Wire3.bytesEncode
        )
        summaries


decodePackageSummaryCacheSharded : List Bytes.Bytes -> Maybe (List CachedPackageModuleSummary)
decodePackageSummaryCacheSharded shardBytes =
    shardBytes
        |> List.foldr
            (\bytes maybeSummaries ->
                case ( Lamdera.Wire3.bytesDecode decodeCachedPackageModuleSummary bytes, maybeSummaries ) of
                    ( Just summary, Just summaries ) ->
                        Just (summary :: summaries)

                    _ ->
                        Nothing
            )
            (Just [])


encodePackageSummaryCacheJson : List CachedPackageModuleSummary -> String
encodePackageSummaryCacheJson summaries =
    summaries
        |> Json.Encode.list encodeCachedPackageModuleSummaryJson
        |> Json.Encode.encode 0


decodePackageSummaryCacheJson : String -> Maybe (List CachedPackageModuleSummary)
decodePackageSummaryCacheJson jsonString =
    Decode.decodeString
        (Decode.list decodeCachedPackageModuleSummaryJson)
        jsonString
        |> Result.toMaybe


packageEnvSeedFromSummaries : List CachedPackageModuleSummary -> PackageEnvSeed
packageEnvSeedFromSummaries summaries =
    { interfaces =
        summaries
            |> List.map (\summary -> ( summary.moduleName, summary.interface ))
    , functionsByModule =
        summaries
            |> List.map
                (\summary ->
                    ( Environment.moduleKey summary.moduleName
                    , summary.functions
                    )
                )
    , importsByModule =
        summaries
            |> List.map
                (\summary ->
                    ( Environment.moduleKey summary.moduleName
                    , summary.importedNames
                    )
                )
    }


encodePackageEnvSeed : PackageEnvSeed -> Bytes.Bytes
encodePackageEnvSeed envSeed =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ Lamdera.Wire3.encodeList encodeInterfaceEntry envSeed.interfaces
        , Lamdera.Wire3.encodeList encodeFunctionModuleEntry envSeed.functionsByModule
        , Lamdera.Wire3.encodeList encodeImportModuleEntry envSeed.importsByModule
        ]
        |> Lamdera.Wire3.bytesEncode


decodePackageEnvSeed : Bytes.Bytes -> Maybe PackageEnvSeed
decodePackageEnvSeed bytes =
    bytes
        |> Lamdera.Wire3.bytesDecode
            (Lamdera.Wire3.succeedDecode PackageEnvSeed
                |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeInterfaceEntry)
                |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeFunctionModuleEntry)
                |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeImportModuleEntry)
            )


ensureDirTask : String -> BackendTask FatalError ()
ensureDirTask dirPath =
    BackendTask.Custom.run "ensureDir"
        (Json.Encode.string dirPath)
        (Decode.succeed ())
        |> BackendTask.allowFatal


{-| Normalize every user module in topological order with per-file caching.

Each module gets its own cache blob keyed on `(packageKey, moduleContentHash)`.
On a code change, only changed files are re-normalized; unchanged files load
from their individual cache. This makes the code-change scenario nearly as fast
as the fully-warm case.

-}
buildUserNormalizedEnv :
    { cacheDir : Maybe String
    , packageKey : String
    , userModulePlans : List UserNormModulePlan
    , userFileContents : Dict String String
    , userNormalizationFlags : NormalizationFlags.NormalizationFlags
    }
    -> Eval.Module.ProjectEnv
    -> BackendTask FatalError UserNormBuildResult
buildUserNormalizedEnv config envBeforeNorm =
    case config.cacheDir of
        Nothing ->
            normalizeAllUserModulesFresh config.userNormalizationFlags config.userModulePlans envBeforeNorm
                |> BackendTask.succeed

        Just cacheDir ->
            normalizePerFile cacheDir config.packageKey config.userNormalizationFlags config.userModulePlans config.userFileContents envBeforeNorm


userNormPlanModuleName : UserNormModulePlan -> ModuleName
userNormPlanModuleName plan =
    Eval.Module.fileModuleName plan.file


userNormPlanAttemptedFunctions : UserNormModulePlan -> Set String
userNormPlanAttemptedFunctions plan =
    case plan.targetFunctions of
        Nothing ->
            FunctionReachability.findTopLevelNames plan.file

        Just targetFunctions ->
            targetFunctions


extendUserNormCacheEntry :
    UserNormCacheEntry
    -> FastDict.Dict String FunctionImplementation
    -> FastDict.Dict String Types.Value
    -> Set String
    -> UserNormCacheEntry
extendUserNormCacheEntry entry newFunctions newPrecomputed attemptedFunctions =
    let
        mergedFunctions : FastDict.Dict String FunctionImplementation
        mergedFunctions =
            entry.functions
                |> List.foldl
                    (\functionImplementation acc ->
                        FastDict.insert (Node.value functionImplementation.name) functionImplementation acc
                    )
                    FastDict.empty
                |> (\existingFunctions ->
                        newFunctions
                            |> FastDict.toList
                            |> List.foldl
                                (\( name, functionImplementation ) acc -> FastDict.insert name functionImplementation acc)
                                existingFunctions
                   )

        mergedPrecomputed : FastDict.Dict String Types.Value
        mergedPrecomputed =
            entry.precomputedValues
                |> List.foldl
                    (\( name, value ) acc -> FastDict.insert name value acc)
                    FastDict.empty
                |> (\existingPrecomputed ->
                        newPrecomputed
                            |> FastDict.toList
                            |> List.foldl
                                (\( name, value ) acc -> FastDict.insert name value acc)
                                existingPrecomputed
                   )
    in
    { moduleName = entry.moduleName
    , functions = FastDict.values mergedFunctions
    , precomputedValues = FastDict.toList mergedPrecomputed
    , attemptedFunctions =
        Set.union (Set.fromList entry.attemptedFunctions) attemptedFunctions
            |> Set.toList
    }


missingUserNormTargets : UserNormModulePlan -> UserNormCacheEntry -> Set String
missingUserNormTargets plan entry =
    Set.diff
        (userNormPlanAttemptedFunctions plan)
        (Set.fromList entry.attemptedFunctions)


writeUserNormEntry : String -> UserNormCacheEntry -> BackendTask FatalError ()
writeUserNormEntry path entry =
    writeBinaryFile
        { path = path
        , bytes = encodeUserNormBundle [ entry ]
        }


emptyUserNormStats : UserNormStats
emptyUserNormStats =
    { modulesPlanned = 0
    , targetFunctions = 0
    , cacheHitModules = 0
    , cacheMissModules = 0
    , cacheExtendedModules = 0
    , rewrittenFunctions = 0
    , precomputedValues = 0
    , dependencySummaryStats = Eval.Module.emptyDependencySummaryStats
    }


combineUserNormStats : UserNormStats -> UserNormStats -> UserNormStats
combineUserNormStats left right =
    { modulesPlanned = left.modulesPlanned + right.modulesPlanned
    , targetFunctions = left.targetFunctions + right.targetFunctions
    , cacheHitModules = left.cacheHitModules + right.cacheHitModules
    , cacheMissModules = left.cacheMissModules + right.cacheMissModules
    , cacheExtendedModules = left.cacheExtendedModules + right.cacheExtendedModules
    , rewrittenFunctions = left.rewrittenFunctions + right.rewrittenFunctions
    , precomputedValues = left.precomputedValues + right.precomputedValues
    , dependencySummaryStats =
        Eval.Module.mergeDependencySummaryStats left.dependencySummaryStats right.dependencySummaryStats
    }


userNormPlanStats : UserNormModulePlan -> UserNormStats
userNormPlanStats plan =
    { emptyUserNormStats
        | modulesPlanned = 1
        , targetFunctions = Set.size (userNormPlanAttemptedFunctions plan)
    }


userNormEntryStats : UserNormCacheEntry -> Eval.Module.DependencySummaryStats -> UserNormStats
userNormEntryStats entry dependencySummaryStats =
    { emptyUserNormStats
        | rewrittenFunctions = List.length entry.functions
        , precomputedValues = List.length entry.precomputedValues
        , dependencySummaryStats = dependencySummaryStats
    }


normalizePerFile :
    String
    -> String
    -> NormalizationFlags.NormalizationFlags
    -> List UserNormModulePlan
    -> Dict String String
    -> Eval.Module.ProjectEnv
    -> BackendTask FatalError UserNormBuildResult
normalizePerFile cacheDir packageKey userNormalizationFlags userModulePlans userFileContents envBeforeNorm =
    -- Refactor (2026-04-21): the original implementation did `File.exists`
    -- then `File.binaryFile` per module inside a serial fold. Each
    -- BackendTask round-trips through the elm-pages scheduler (Elm ↔ JS),
    -- and 63 modules × 2 round-trips ≈ 1 s of pure scheduler latency on
    -- elm-review even though the underlying disk I/O takes <10 ms total.
    --
    -- Now: batch all per-module `exists+read` tasks via
    -- `BackendTask.Extra.combine` (one combined round-trip in practice),
    -- then run the serial fold over the pre-read `Maybe Bytes` results.
    -- The fold still needs to be sequential because cache-miss / extension
    -- branches normalize against the running `env` (one module's
    -- normalization may use the prior module's bodies), but reads no
    -- longer add round-trip overhead.
    Do.do (ensureDirTask cacheDir) <| \_ ->
        let
            flagsKey =
                userNormalizationFlagsKey userNormalizationFlags

            plansWithPaths : List ( UserNormModulePlan, String )
            plansWithPaths =
                userModulePlans
                    |> List.map
                        (\plan ->
                            let
                                moduleKey : String
                                moduleKey =
                                    String.join "." (userNormPlanModuleName plan)

                                contentHash : String
                                contentHash =
                                    Dict.get moduleKey userFileContents
                                        |> Maybe.map (FNV1a.hash >> String.fromInt)
                                        |> Maybe.withDefault "0"
                            in
                            ( plan
                            , cacheDir
                                ++ "/user-norm-"
                                ++ userNormCacheVersion
                                ++ "-"
                                ++ packageKey
                                ++ "-"
                                ++ flagsKey
                                ++ "-"
                                ++ moduleKey
                                ++ "-"
                                ++ contentHash
                                ++ ".blob"
                            )
                        )

            readCachedBytesTask : BackendTask FatalError (List (Maybe Bytes.Bytes))
            readCachedBytesTask =
                -- `File.optional` collapses the missing-file case to
                -- `Nothing` via `binaryFile`'s recoverable
                -- `FileDoesntExist` error. Saves one batched scheduler
                -- round-trip per `normalizePerFile` call vs the prior
                -- `File.exists` + conditional read (`exists` had to
                -- resolve before the scheduler could fire the read).
                plansWithPaths
                    |> List.map (\( _, path ) -> File.binaryFile path |> File.optional)
                    |> BackendTask.Extra.combine
        in
        Do.do readCachedBytesTask <| \maybeCachedBytesList ->
            List.map2 Tuple.pair plansWithPaths maybeCachedBytesList
                |> List.foldl
                    (\( ( userModulePlan, perFilePath ), maybeBytes ) resultTask ->
                        Do.do resultTask <|
                            \resultAcc ->
                                let
                                    planStats : UserNormStats
                                    planStats =
                                        userNormPlanStats userModulePlan
                                in
                                case maybeBytes of
                                    Just bytes ->
                                        case decodeUserNormBundle bytes of
                                            Just [ entry ] ->
                                                let
                                                    envFromCache : Eval.Module.ProjectEnv
                                                    envFromCache =
                                                        applyUserNormBundle [ entry ] resultAcc.env

                                                    missingTargets : Set String
                                                    missingTargets =
                                                        missingUserNormTargets userModulePlan entry
                                                in
                                                if Set.isEmpty missingTargets then
                                                    BackendTask.succeed
                                                        { env = envFromCache
                                                        , stats =
                                                            combineUserNormStats resultAcc.stats
                                                                { planStats | cacheHitModules = 1 }
                                                        }

                                                else
                                                    let
                                                        extensionPlan : UserNormModulePlan
                                                        extensionPlan =
                                                            { file = userModulePlan.file
                                                            , targetFunctions = Just missingTargets
                                                            }

                                                        extensionResult =
                                                            normalizeOneAndCache userNormalizationFlags extensionPlan envFromCache

                                                        mergedEntry : UserNormCacheEntry
                                                        mergedEntry =
                                                            extendUserNormCacheEntry entry
                                                                (extensionResult.entry.functions
                                                                    |> List.map (\functionImplementation -> ( Node.value functionImplementation.name, functionImplementation ))
                                                                    |> FastDict.fromList
                                                                )
                                                                (extensionResult.entry.precomputedValues |> FastDict.fromList)
                                                                missingTargets
                                                    in
                                                    writeUserNormEntry perFilePath mergedEntry
                                                        |> BackendTask.map
                                                            (\_ ->
                                                                { env = extensionResult.env
                                                                , stats =
                                                                    combineUserNormStats resultAcc.stats
                                                                        (combineUserNormStats
                                                                            { planStats
                                                                                | cacheHitModules = 1
                                                                                , cacheExtendedModules = 1
                                                                            }
                                                                            (userNormEntryStats extensionResult.entry extensionResult.dependencySummaryStats)
                                                                        )
                                                                }
                                                            )

                                            _ ->
                                                let
                                                    normalizeResult =
                                                        normalizeOneAndCache userNormalizationFlags userModulePlan resultAcc.env
                                                in
                                                writeUserNormEntry perFilePath normalizeResult.entry
                                                    |> BackendTask.map
                                                        (\_ ->
                                                            { env = normalizeResult.env
                                                            , stats =
                                                                combineUserNormStats resultAcc.stats
                                                                    (combineUserNormStats
                                                                        { planStats | cacheMissModules = 1 }
                                                                        (userNormEntryStats normalizeResult.entry normalizeResult.dependencySummaryStats)
                                                                    )
                                                            }
                                                        )

                                    Nothing ->
                                        let
                                            normalizeResult =
                                                normalizeOneAndCache userNormalizationFlags userModulePlan resultAcc.env
                                        in
                                        writeUserNormEntry perFilePath normalizeResult.entry
                                            |> BackendTask.map
                                                (\_ ->
                                                    { env = normalizeResult.env
                                                    , stats =
                                                        combineUserNormStats resultAcc.stats
                                                            (combineUserNormStats
                                                                { planStats | cacheMissModules = 1 }
                                                                (userNormEntryStats normalizeResult.entry normalizeResult.dependencySummaryStats)
                                                            )
                                                    }
                                                )
                    )
                    (BackendTask.succeed
                        { env = envBeforeNorm
                        , stats = emptyUserNormStats
                        }
                    )


normalizeOneAndCache :
    NormalizationFlags.NormalizationFlags
    -> UserNormModulePlan
    -> Eval.Module.ProjectEnv
    ->
        { env : Eval.Module.ProjectEnv
        , entry : UserNormCacheEntry
        , dependencySummaryStats : Eval.Module.DependencySummaryStats
        }
normalizeOneAndCache userNormalizationFlags userModulePlan envAcc =
    let
        moduleName : ModuleName
        moduleName =
            userNormPlanModuleName userModulePlan

        normalizationResult =
            Eval.Module.normalizeOneModuleInEnvSelectedWithFlags userNormalizationFlags userModulePlan.targetFunctions moduleName envAcc

        entry =
            { moduleName = moduleName
            , functions = FastDict.values normalizationResult.delta
            , precomputedValues = FastDict.toList normalizationResult.precomputed
            , attemptedFunctions = userNormPlanAttemptedFunctions userModulePlan |> Set.toList
            }
    in
    { env = normalizationResult.env
    , entry = entry
    , dependencySummaryStats = normalizationResult.stats
    }


{-| Normalize every user module in topo order without consulting a cache.
Returns the updated env alongside the list of `UserNormCacheEntry` records
we can feed to the encoder.
-}
normalizeAllUserModulesFresh :
    NormalizationFlags.NormalizationFlags
    -> List UserNormModulePlan
    -> Eval.Module.ProjectEnv
    -> UserNormBuildResult
normalizeAllUserModulesFresh userNormalizationFlags userModulePlans envBeforeNorm =
    userModulePlans
        |> List.foldl
            (\userModulePlan resultAcc ->
                let
                    planStats : UserNormStats
                    planStats =
                        userNormPlanStats userModulePlan

                    normalizeResult =
                        normalizeOneAndCache userNormalizationFlags userModulePlan resultAcc.env
                in
                { env = normalizeResult.env
                , stats =
                    combineUserNormStats resultAcc.stats
                        (combineUserNormStats
                            { planStats | cacheMissModules = 1 }
                            (userNormEntryStats normalizeResult.entry normalizeResult.dependencySummaryStats)
                        )
                }
            )
            { env = envBeforeNorm
            , stats = emptyUserNormStats
            }


{-| Install a decoded `UserNormCacheEntry` list into the env. Each entry
contains the functions whose bodies were rewritten during normalization (overlaid
onto the originals) AND the precomputed constant values.
-}
applyUserNormBundle : List UserNormCacheEntry -> Eval.Module.ProjectEnv -> Eval.Module.ProjectEnv
applyUserNormBundle entries env =
    List.foldl
        (\entry currentEnv ->
            let
                deltaDict : FastDict.Dict String FunctionImplementation
                deltaDict =
                    entry.functions
                        |> List.map (\f -> ( Node.value f.name, f ))
                        |> FastDict.fromList

                precomputedDict : FastDict.Dict String Types.Value
                precomputedDict =
                    FastDict.fromList entry.precomputedValues
            in
            currentEnv
                |> Eval.Module.mergeModuleFunctionsIntoEnv entry.moduleName deltaDict
                |> Eval.Module.setModulePrecomputedValues entry.moduleName precomputedDict
        )
        env
        entries


writeBinaryFile : { path : String, bytes : Bytes.Bytes } -> BackendTask FatalError ()
writeBinaryFile { path, bytes } =
    BackendTask.Custom.run "writeBinaryFile"
        (Json.Encode.object
            [ ( "path", Json.Encode.string path )
            , ( "bytes"
              , bytes
                    |> Lamdera.Wire3.intListFromBytes
                    |> Json.Encode.list Json.Encode.int
              )
            ]
        )
        (Decode.succeed ())
        |> BackendTask.allowFatal


encodeCachedPackageModuleSummary : CachedPackageModuleSummary -> Lamdera.Wire3.Encoder
encodeCachedPackageModuleSummary summary =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ encodeModuleName summary.moduleName
        , Lamdera.Wire3.encodeList encodeExposed summary.interface
        , encodeImportedNames summary.importedNames
        , Lamdera.Wire3.encodeList encodeFunctionImplementationNoRanges summary.functions
        ]


decodeCachedPackageModuleSummary : Lamdera.Wire3.Decoder CachedPackageModuleSummary
decodeCachedPackageModuleSummary =
    Lamdera.Wire3.succeedDecode
        (\moduleName interface importedNames functions ->
            { moduleName = moduleName
            , interface = interface
            , importedNames = importedNames
            , functions = functions
            }
        )
        |> Lamdera.Wire3.andMapDecode decodeModuleName
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeExposed)
        |> Lamdera.Wire3.andMapDecode decodeImportedNames
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeFunctionImplementationNoRanges)


encodeCachedPackageModuleSummaryJson : CachedPackageModuleSummary -> Json.Encode.Value
encodeCachedPackageModuleSummaryJson summary =
    Json.Encode.object
        [ ( "moduleName", Json.Encode.list Json.Encode.string summary.moduleName )
        , ( "interface", Json.Encode.list encodeExposedJson summary.interface )
        , ( "importedNames", encodeImportedNamesJson summary.importedNames )
        , ( "functions", Json.Encode.list encodeFunctionImplementationNoRangesJson summary.functions )
        ]


decodeCachedPackageModuleSummaryJson : Decode.Decoder CachedPackageModuleSummary
decodeCachedPackageModuleSummaryJson =
    Decode.map4
        (\moduleName interface importedNames functions ->
            { moduleName = moduleName
            , interface = interface
            , importedNames = importedNames
            , functions = functions
            }
        )
        (Decode.field "moduleName" (Decode.list Decode.string))
        (Decode.field "interface" (Decode.list decodeExposedJson))
        (Decode.field "importedNames" decodeImportedNamesJson)
        (Decode.field "functions" (Decode.list decodeFunctionImplementationNoRangesJson))


encodeModuleName : ModuleName -> Lamdera.Wire3.Encoder
encodeModuleName moduleName =
    Lamdera.Wire3.encodeList Lamdera.Wire3.encodeString moduleName


decodeModuleName : Lamdera.Wire3.Decoder ModuleName
decodeModuleName =
    Lamdera.Wire3.decodeList Lamdera.Wire3.decodeString


encodeExposed : Exposed -> Lamdera.Wire3.Encoder
encodeExposed exposed =
    case exposed of
        Elm.Interface.Function name ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeInt 0
                , Lamdera.Wire3.encodeString name
                ]

        Elm.Interface.CustomType ( name, constructors ) ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeInt 1
                , Lamdera.Wire3.encodeString name
                , Lamdera.Wire3.encodeList Lamdera.Wire3.encodeString constructors
                ]

        Elm.Interface.Alias name ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeInt 2
                , Lamdera.Wire3.encodeString name
                ]

        Elm.Interface.Operator infix_ ->
            Lamdera.Wire3.encodeSequenceWithoutLength
                [ Lamdera.Wire3.encodeInt 3
                , encodeInfixDirectionRaw (Node.value infix_.direction)
                , Lamdera.Wire3.encodeInt (Node.value infix_.precedence)
                , Lamdera.Wire3.encodeString (Node.value infix_.operator)
                , Lamdera.Wire3.encodeString (Node.value infix_.function)
                ]


decodeExposed : Lamdera.Wire3.Decoder Exposed
decodeExposed =
    Lamdera.Wire3.decodeInt
        |> Lamdera.Wire3.andThenDecode
            (\tag ->
                case tag of
                    0 ->
                        Lamdera.Wire3.succeedDecode Elm.Interface.Function
                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString

                    1 ->
                        Lamdera.Wire3.succeedDecode (\name constructors -> Elm.Interface.CustomType ( name, constructors ))
                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                            |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList Lamdera.Wire3.decodeString)

                    2 ->
                        Lamdera.Wire3.succeedDecode Elm.Interface.Alias
                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString

                    _ ->
                        Lamdera.Wire3.succeedDecode
                            (\direction precedence operator_ function_ ->
                                Elm.Interface.Operator
                                    { direction = fakeNode direction
                                    , precedence = fakeNode precedence
                                    , operator = fakeNode operator_
                                    , function = fakeNode function_
                                    }
                            )
                            |> Lamdera.Wire3.andMapDecode decodeInfixDirectionRaw
                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeInt
                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                            |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
            )


encodeExposedJson : Exposed -> Json.Encode.Value
encodeExposedJson exposed =
    case exposed of
        Elm.Interface.Function name ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "function" )
                , ( "name", Json.Encode.string name )
                ]

        Elm.Interface.CustomType ( name, constructors ) ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "custom-type" )
                , ( "name", Json.Encode.string name )
                , ( "constructors", Json.Encode.list Json.Encode.string constructors )
                ]

        Elm.Interface.Alias name ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "alias" )
                , ( "name", Json.Encode.string name )
                ]

        Elm.Interface.Operator infix_ ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "operator" )
                , ( "infix", Infix.encode infix_ )
                ]


decodeExposedJson : Decode.Decoder Exposed
decodeExposedJson =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "function" ->
                        Decode.map Elm.Interface.Function
                            (Decode.field "name" Decode.string)

                    "custom-type" ->
                        Decode.map2 (\name constructors -> Elm.Interface.CustomType ( name, constructors ))
                            (Decode.field "name" Decode.string)
                            (Decode.field "constructors" (Decode.list Decode.string))

                    "alias" ->
                        Decode.map Elm.Interface.Alias
                            (Decode.field "name" Decode.string)

                    "operator" ->
                        Decode.map Elm.Interface.Operator
                            (Decode.field "infix" Infix.decoder)

                    _ ->
                        Decode.fail ("Unknown exposed tag: " ++ tag)
            )


encodeImportedNames : Types.ImportedNames -> Lamdera.Wire3.Encoder
encodeImportedNames importedNames =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ encodeQualifiedMapping importedNames.aliases
        , encodeQualifiedMapping importedNames.exposedValues
        , encodeQualifiedMapping importedNames.exposedConstructors
        ]


decodeImportedNames : Lamdera.Wire3.Decoder Types.ImportedNames
decodeImportedNames =
    Lamdera.Wire3.succeedDecode
        (\aliases exposedValues exposedConstructors ->
            { aliases = aliases
            , exposedValues = exposedValues
            , exposedConstructors = exposedConstructors
            }
        )
        |> Lamdera.Wire3.andMapDecode decodeQualifiedMapping
        |> Lamdera.Wire3.andMapDecode decodeQualifiedMapping
        |> Lamdera.Wire3.andMapDecode decodeQualifiedMapping


encodeImportedNamesJson : Types.ImportedNames -> Json.Encode.Value
encodeImportedNamesJson importedNames =
    Json.Encode.object
        [ ( "aliases", encodeQualifiedMappingJson importedNames.aliases )
        , ( "exposedValues", encodeQualifiedMappingJson importedNames.exposedValues )
        , ( "exposedConstructors", encodeQualifiedMappingJson importedNames.exposedConstructors )
        ]


decodeImportedNamesJson : Decode.Decoder Types.ImportedNames
decodeImportedNamesJson =
    Decode.map3
        (\aliases exposedValues exposedConstructors ->
            { aliases = aliases
            , exposedValues = exposedValues
            , exposedConstructors = exposedConstructors
            }
        )
        (Decode.field "aliases" decodeQualifiedMappingJson)
        (Decode.field "exposedValues" decodeQualifiedMappingJson)
        (Decode.field "exposedConstructors" decodeQualifiedMappingJson)


encodeQualifiedMapping : FastDict.Dict String ( ModuleName, String ) -> Lamdera.Wire3.Encoder
encodeQualifiedMapping mapping =
    mapping
        |> FastDict.toList
        |> Lamdera.Wire3.encodeList
            (\( name, ( moduleName, _ ) ) ->
                Lamdera.Wire3.encodeSequenceWithoutLength
                    [ Lamdera.Wire3.encodeString name
                    , encodeModuleName moduleName
                    ]
            )


decodeQualifiedMapping : Lamdera.Wire3.Decoder (FastDict.Dict String ( ModuleName, String ))
decodeQualifiedMapping =
    Lamdera.Wire3.succeedDecode FastDict.fromList
        |> Lamdera.Wire3.andMapDecode
            (Lamdera.Wire3.decodeList
                (Lamdera.Wire3.succeedDecode
                    (\name moduleName ->
                        ( name, ( moduleName, Environment.moduleKey moduleName ) )
                    )
                    |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
                    |> Lamdera.Wire3.andMapDecode decodeModuleName
                )
            )


encodeQualifiedMappingJson : FastDict.Dict String ( ModuleName, String ) -> Json.Encode.Value
encodeQualifiedMappingJson mapping =
    mapping
        |> FastDict.toList
        |> Json.Encode.list
            (\( name, ( moduleName, _ ) ) ->
                Json.Encode.object
                    [ ( "name", Json.Encode.string name )
                    , ( "moduleName", Json.Encode.list Json.Encode.string moduleName )
                    ]
            )


decodeQualifiedMappingJson : Decode.Decoder (FastDict.Dict String ( ModuleName, String ))
decodeQualifiedMappingJson =
    Decode.map FastDict.fromList
        (Decode.list
            (Decode.map2
                (\name moduleName ->
                    ( name, ( moduleName, Environment.moduleKey moduleName ) )
                )
                (Decode.field "name" Decode.string)
                (Decode.field "moduleName" (Decode.list Decode.string))
            )
        )


encodeInterfaceEntry : ( ModuleName, List Exposed ) -> Lamdera.Wire3.Encoder
encodeInterfaceEntry ( moduleName, interface ) =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ encodeModuleName moduleName
        , Lamdera.Wire3.encodeList encodeExposed interface
        ]


decodeInterfaceEntry : Lamdera.Wire3.Decoder ( ModuleName, List Exposed )
decodeInterfaceEntry =
    Lamdera.Wire3.succeedDecode Tuple.pair
        |> Lamdera.Wire3.andMapDecode decodeModuleName
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeExposed)


encodeFunctionModuleEntry : ( String, List FunctionImplementation ) -> Lamdera.Wire3.Encoder
encodeFunctionModuleEntry ( moduleKey, functions ) =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ Lamdera.Wire3.encodeString moduleKey
        , Lamdera.Wire3.encodeList encodeFunctionImplementationNoRanges functions
        ]


decodeFunctionModuleEntry : Lamdera.Wire3.Decoder ( String, List FunctionImplementation )
decodeFunctionModuleEntry =
    Lamdera.Wire3.succeedDecode Tuple.pair
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList decodeFunctionImplementationNoRanges)


encodeImportModuleEntry : ( String, Types.ImportedNames ) -> Lamdera.Wire3.Encoder
encodeImportModuleEntry ( moduleKey, importedNames ) =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ Lamdera.Wire3.encodeString moduleKey
        , encodeImportedNames importedNames
        ]


decodeImportModuleEntry : Lamdera.Wire3.Decoder ( String, Types.ImportedNames )
decodeImportModuleEntry =
    Lamdera.Wire3.succeedDecode Tuple.pair
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
        |> Lamdera.Wire3.andMapDecode decodeImportedNames


encodeFunctionImplementationNoRanges : FunctionImplementation -> Lamdera.Wire3.Encoder
encodeFunctionImplementationNoRanges implementation =
    Lamdera.Wire3.encodeSequenceWithoutLength
        [ Lamdera.Wire3.encodeString (Node.value implementation.name)
        , Lamdera.Wire3.encodeList (AstWireCodec.encodePattern << Node.value) implementation.arguments
        , AstWireCodec.encodeExpression (Node.value implementation.expression)
        ]


decodeFunctionImplementationNoRanges : Lamdera.Wire3.Decoder FunctionImplementation
decodeFunctionImplementationNoRanges =
    Lamdera.Wire3.succeedDecode
        (\name arguments expression ->
            { name = fakeNode name
            , arguments = List.map fakeNode arguments
            , expression = fakeNode expression
            }
        )
        |> Lamdera.Wire3.andMapDecode Lamdera.Wire3.decodeString
        |> Lamdera.Wire3.andMapDecode (Lamdera.Wire3.decodeList AstWireCodec.decodePattern)
        |> Lamdera.Wire3.andMapDecode AstWireCodec.decodeExpression


encodeFunctionImplementationNoRangesJson : FunctionImplementation -> Json.Encode.Value
encodeFunctionImplementationNoRangesJson implementation =
    Elm.Syntax.Expression.encodeFunction
        { documentation = Nothing
        , signature = Nothing
        , declaration = fakeNode implementation
        }


decodeFunctionImplementationNoRangesJson : Decode.Decoder FunctionImplementation
decodeFunctionImplementationNoRangesJson =
    Elm.Syntax.Expression.functionDecoder
        |> Decode.map (\function_ -> Node.value function_.declaration)


encodeInfixDirectionRaw : InfixDirection -> Lamdera.Wire3.Encoder
encodeInfixDirectionRaw direction =
    case direction of
        Left ->
            Lamdera.Wire3.encodeInt 0

        Right ->
            Lamdera.Wire3.encodeInt 1

        Non ->
            Lamdera.Wire3.encodeInt 2


decodeInfixDirectionRaw : Lamdera.Wire3.Decoder InfixDirection
decodeInfixDirectionRaw =
    Lamdera.Wire3.decodeInt
        |> Lamdera.Wire3.andThenDecode
            (\tag ->
                case tag of
                    0 ->
                        Lamdera.Wire3.succeedDecode Left

                    1 ->
                        Lamdera.Wire3.succeedDecode Right

                    _ ->
                        Lamdera.Wire3.succeedDecode Non
            )


type InterpreterProject
    = InterpreterProject
        { sourceDirectories : List String
        , inputsByPath : Dict String ( Path, Cache.Monad FileOrDirectory )
        , userFileContents : Dict String String
        , depGraph : DepGraph.Graph
        , patchedPackageSources : List String
        , extraSources : List String
        , moduleGraph : ModuleGraph
        , packageModuleNames : Set String
        , packageEnv : Eval.Module.ProjectEnv
        , baseUserEnv : Maybe Eval.Module.ProjectEnv
        , semanticIndex : SemanticHash.DeclarationIndex
        , envMode : EnvMode
        }


interpreterResultCacheVersion : String
interpreterResultCacheVersion =
    "v2"


{-| Selects the interpreter environment representation for this project's
evaluations.

  - `LegacyAst` — the old AST-based evaluator with `Dict String Value` locals.
    The canonical production path; same behavior this project has always had.
  - `ResolvedListUnplanned` — routes through the resolved IR evaluator
    (`Eval.Module.evalWithResolvedIRFromFilesAndIntercepts`). Closures still
    capture the whole enclosing locals list; no slot planning. Used as an
    A/B baseline against `LegacyAst` to isolate "does finishing the IR
    integration help on its own" from the slot refactor.
  - `ResolvedListSlotted` — resolved IR + copy-on-capture closures driven by
    resolver-emitted `captureSlots`. In Phase 1 this behaves identically to
    `ResolvedListUnplanned`; Phase 3 adds the slot-refactor semantics.

The mode is stored on the `InterpreterProject` opaque type and read by
the eval entry points. Callers set it once after `load`/`loadWith` via
`withEnvMode`. Defaulting to `LegacyAst` preserves existing behavior for
every caller that hasn't opted in.

-}
type EnvMode
    = LegacyAst
    | ResolvedListUnplanned
    | ResolvedListSlotted


{-| Attach an `EnvMode` to a loaded `InterpreterProject`. Calling this
lets downstream eval entry points route through the resolved IR path
(`ResolvedListUnplanned` / `ResolvedListSlotted`) instead of the default
`LegacyAst` path. Idempotent and cheap — just updates one field on the
opaque record.
-}
withEnvMode : EnvMode -> InterpreterProject -> InterpreterProject
withEnvMode mode (InterpreterProject project) =
    InterpreterProject { project | envMode = mode }


{-| Initialize an InterpreterProject with default settings.

Reads elm.json for source directories, loads all package dependencies,
and sets up the project for interpreter evaluation.

-}
load :
    { projectDir : Path }
    -> BackendTask FatalError InterpreterProject
load { projectDir } =
    loadWith
        { projectDir = projectDir
        , skipPackages = Set.empty
        , patchSource = identity
        , patchUserSource = \_ source -> source
        , extraSourceFiles = []
        , extraReachableImports = []
        , sourceDirectories = Nothing
        , normalizationRoots = Nothing
        }


{-| Initialize an InterpreterProject with advanced options.

  - `skipPackages` — package names to exclude (e.g. those with kernel code)

  - `patchSource` — transform applied to each package source after loading

  - `extraSourceFiles` — additional source files to include

  - `sourceDirectories` — `Nothing` reads from elm.json, `Just` overrides

  - `normalizationRoots` — list of user module names whose top-level
    functions are the evaluation entry points. When `Just`, the loader
    runs a function-level reachability walk (see
    `FunctionReachability.computeReachable`) from those modules and only
    normalizes user modules containing a reachable function. Modules
    outside the live set stay parsed-but-unnormalized: they're still
    loaded into `ProjectEnv` so runtime lookups work, they just skip
    the expensive eager `tryNormalizeConstant` pass on cold-user load.

    Cache interaction: the `user-norm-v4-*.blob` cache key is the
    per-module content hash, independent of the reachability set. Cache
    hits always apply; `normalizationRoots` only gates whether a cache
    MISS does work. Over time the on-disk cache fills up regardless of
    which plan any individual run uses.

    `Nothing` means "no plan information, normalize every user module
    eagerly" — the safe conservative default for raw loaders,
    interactive REPL evaluation, and anything where the eventual
    evaluation plan isn't known at load time.

    Callers that DO know their plan upfront should pass it: test runners
    pass test module names, ReviewRunner passes `["ReviewConfig"]`,
    benchmark runners pass their fixture modules. See
    `src/FunctionReachability.elm` for the walk semantics.

-}
loadWith :
    { projectDir : Path
    , skipPackages : Set String
    , patchSource : String -> String
    , patchUserSource : String -> String -> String
    , extraSourceFiles : List String
    , extraReachableImports : List String
    , sourceDirectories : Maybe (List String)
    , normalizationRoots : Maybe (List String)
    }
    -> BackendTask FatalError InterpreterProject
loadWith config =
    loadWithUserNormalizationFlags NormalizationFlags.experimental config


{-| Like `loadWith`, but skips the `build_graph` phase by using a pre-built
`depGraph` + `moduleGraph` supplied by the caller.

Intended for parallel-worker pools where the main thread loads the
project once (computing graphs from scratch) and ships the resulting
graph state to workers via `BackendTask.Parallel.initShared`. Workers
then call `loadWithPreBuiltGraphs` with the shipped graphs to skip the
two expensive parts of `build_graph`:

  1. `DepGraph.buildGraph` over user sources.
  2. `Elm.Parser.parseToFile` for every user file (populates
     `moduleGraph.moduleToFile`).

Workers still load package sources, glob/read user files, and re-derive
cheap downstream values (package module names, topo sort over packages)
since those depend on inputs the worker has anyway. The save is bounded
by `buildGraphMs` minus the small residual.

It's the caller's responsibility to make sure `preBuiltGraphs` was
produced from the same `projectDir`, `sourceDirectories`, `patchSource`,
and `patchUserSource` settings — mismatches will silently produce a
project whose graphs disagree with its sources.
-}
loadWithPreBuiltGraphs :
    { projectDir : Path
    , skipPackages : Set String
    , patchSource : String -> String
    , patchUserSource : String -> String -> String
    , extraSourceFiles : List String
    , extraReachableImports : List String
    , sourceDirectories : Maybe (List String)
    , normalizationRoots : Maybe (List String)
    , preBuiltDepGraph : Maybe DepGraph.Graph
    , preBuiltModuleGraph : Maybe ModuleGraph
    }
    -> BackendTask FatalError InterpreterProject
loadWithPreBuiltGraphs config =
    loadWithProfileUserNormalizationFlags NormalizationFlags.experimental
        { projectDir = config.projectDir
        , skipPackages = config.skipPackages
        , patchSource = config.patchSource
        , patchUserSource = config.patchUserSource
        , extraSourceFiles = config.extraSourceFiles
        , extraReachableImports = config.extraReachableImports
        , sourceDirectories = config.sourceDirectories
        , normalizationRoots = config.normalizationRoots
        , packageParseCacheDir =
            Just (Path.toString config.projectDir ++ "/.elm-build")
        , preBuiltDepGraph = config.preBuiltDepGraph
        , preBuiltModuleGraph = config.preBuiltModuleGraph
        }
        |> BackendTask.map .project


loadWithUserNormalizationFlags :
    NormalizationFlags.NormalizationFlags
    ->
        { projectDir : Path
        , skipPackages : Set String
        , patchSource : String -> String
        , patchUserSource : String -> String -> String
        , extraSourceFiles : List String
        , extraReachableImports : List String
        , sourceDirectories : Maybe (List String)
        , normalizationRoots : Maybe (List String)
        }
    -> BackendTask FatalError InterpreterProject
loadWithUserNormalizationFlags userNormalizationFlags config =
    loadWithProfileUserNormalizationFlags userNormalizationFlags
        { projectDir = config.projectDir
        , skipPackages = config.skipPackages
        , patchSource = config.patchSource
        , patchUserSource = config.patchUserSource
        , extraSourceFiles = config.extraSourceFiles
        , extraReachableImports = config.extraReachableImports
        , sourceDirectories = config.sourceDirectories
        , normalizationRoots = config.normalizationRoots
        , packageParseCacheDir =
            -- Default to the project's `.elm-build` directory so the package
            -- summary cache (including normalized top-level constants) is
            -- persisted across runs without every caller having to opt in.
            Just (Path.toString config.projectDir ++ "/.elm-build")
        , preBuiltDepGraph = Nothing
        , preBuiltModuleGraph = Nothing
        }
        |> BackendTask.map .project


loadWithProfile :
    { projectDir : Path
    , skipPackages : Set String
    , patchSource : String -> String
    , patchUserSource : String -> String -> String
    , extraSourceFiles : List String
    , extraReachableImports : List String
    , sourceDirectories : Maybe (List String)
    , normalizationRoots : Maybe (List String)
    , packageParseCacheDir : Maybe String
    , preBuiltDepGraph : Maybe DepGraph.Graph
    , preBuiltModuleGraph : Maybe ModuleGraph
    }
    -> BackendTask FatalError { project : InterpreterProject, profile : LoadProfile }
loadWithProfile config =
    loadWithProfileUserNormalizationFlags NormalizationFlags.experimental config


loadWithProfileUserNormalizationFlags :
    NormalizationFlags.NormalizationFlags
    ->
        { projectDir : Path
        , skipPackages : Set String
        , patchSource : String -> String
        , patchUserSource : String -> String -> String
        , extraSourceFiles : List String
        , extraReachableImports : List String
        , sourceDirectories : Maybe (List String)
        , normalizationRoots : Maybe (List String)
        , packageParseCacheDir : Maybe String
        , preBuiltDepGraph : Maybe DepGraph.Graph
        , preBuiltModuleGraph : Maybe ModuleGraph
        }
    -> BackendTask FatalError { project : InterpreterProject, profile : LoadProfile }
loadWithProfileUserNormalizationFlags userNormalizationFlags config =
    Do.do
        (withTiming
            (case config.sourceDirectories of
                Just dirs ->
                    BackendTask.succeed dirs

                Nothing ->
                    readSourceDirectories config.projectDir
            )
        )
    <|
        \sourceDirectoriesTimed ->
            let
                sourceDirectories =
                    sourceDirectoriesTimed.value
            in
            Do.do
                (withTiming
                    (ProjectSources.loadPackageDepsCached
                        { projectDir = config.projectDir
                        , skipPackages = config.skipPackages
                        }
                    )
                )
            <|
                \packageSourcesTimed ->
                    let
                        patchedPackageSources : List String
                        patchedPackageSources =
                            List.map config.patchSource packageSourcesTimed.value

                        userSourceGlobs : List String
                        userSourceGlobs =
                            sourceDirectories
                                |> List.map (\dir -> dir ++ "/**/*.elm")
                    in
                    Do.do
                        (withTiming
                            (userSourceGlobs
                                |> List.map
                                    (\globPattern ->
                                        Glob.fromStringWithOptions
                                            (let
                                                o : Glob.Options
                                                o =
                                                    Glob.defaultOptions
                                             in
                                             { o | include = Glob.OnlyFiles }
                                            )
                                            globPattern
                                    )
                                |> BackendTask.Extra.combine
                                |> BackendTask.map (List.concat >> List.sort)
                            )
                        )
                    <|
                        \elmFilesTimed ->
                            let
                                elmFiles =
                                    elmFilesTimed.value
                            in
                            Do.do
                                (withTiming
                                    (readFilesTask elmFiles
                                        |> BackendTask.map (List.map2 Tuple.pair elmFiles)
                                    )
                                )
                            <|
                                \userFileContentsTimed ->
                                    let
                                        userFileContents =
                                            userFileContentsTimed.value
                                                |> List.map
                                                    (\( filePath, content ) ->
                                                        ( filePath
                                                        , config.patchUserSource filePath content
                                                        )
                                                    )
                                    in
                                    Do.do
                                        (withTiming
                                            (readFilesTask config.extraSourceFiles
                                                |> BackendTask.map (List.map2 Tuple.pair config.extraSourceFiles)
                                            )
                                        )
                                    <|
                                        \extraFileContentsTimed ->
                                            let
                                                extraFileContents =
                                                    extraFileContentsTimed.value
                                            in
                                            Do.do BackendTask.Time.now <|
                                                \graphStart ->
                                                    let
                                                        allUserPaths : List Path
                                                        allUserPaths =
                                                            elmFiles
                                                                |> List.map Path.path

                                                        allSourceStrings : List String
                                                        allSourceStrings =
                                                            patchedPackageSources
                                                                ++ List.map Tuple.second extraFileContents
                                                                ++ List.map Tuple.second userFileContents

                                                        allModules : List ( String, String )
                                                        allModules =
                                                            List.filterMap
                                                                (\src ->
                                                                    DepGraph.parseModuleName src
                                                                        |> Maybe.map (\name -> ( name, src ))
                                                                )
                                                                allSourceStrings

                                                        -- Pre-built `depGraph` / `moduleGraph` skip the two
                                                        -- expensive parts of `build_graph`: the dep-graph
                                                        -- construction and the per-user-file `Elm.Parser`
                                                        -- pass that populates `moduleGraph.moduleToFile`.
                                                        -- They're independent Maybes so callers can ship
                                                        -- just one (e.g. depGraph alone is tiny on the
                                                        -- wire — useful for measuring the marginal
                                                        -- benefit without the heavy `File`-AST decode
                                                        -- cost). The remaining derivations (allModules,
                                                        -- pkgModuleNames, topo sort over packages) are
                                                        -- cheap and still run on workers — they need
                                                        -- access to `patchedPackageSources` anyway, which
                                                        -- workers still load from disk.
                                                        depGraph : DepGraph.Graph
                                                        depGraph =
                                                            case config.preBuiltDepGraph of
                                                                Just g ->
                                                                    g

                                                                Nothing ->
                                                                    DepGraph.buildGraph
                                                                        { sourceDirectories = sourceDirectories
                                                                        , files =
                                                                            userFileContents
                                                                                |> List.map
                                                                                    (\( filePath, content ) ->
                                                                                        { filePath = filePath
                                                                                        , content = content
                                                                                        }
                                                                                    )
                                                                        }

                                                        moduleGraph : ModuleGraph
                                                        moduleGraph =
                                                            case config.preBuiltModuleGraph of
                                                                Just mg ->
                                                                    mg

                                                                Nothing ->
                                                                    { moduleToSource = Dict.fromList allModules
                                                                    , moduleToFile =
                                                                        userFileContents
                                                                            |> List.filterMap
                                                                                (\( _, content ) ->
                                                                                    case Elm.Parser.parseToFile content of
                                                                                        Ok file ->
                                                                                            DepGraph.parseModuleName content
                                                                                                |> Maybe.map (\name -> ( name, file ))

                                                                                        Err _ ->
                                                                                            Nothing
                                                                                )
                                                                            |> Dict.fromList
                                                                    , imports =
                                                                        allModules
                                                                            |> List.map
                                                                                (\( name, src ) ->
                                                                                    ( name, DepGraph.parseImports src |> Set.fromList )
                                                                                )
                                                                            |> Dict.fromList
                                                                    }

                                                        userParsedFiles : Dict String File
                                                        userParsedFiles =
                                                            moduleGraph.moduleToFile

                                                        allStableSources : List String
                                                        allStableSources =
                                                            patchedPackageSources ++ List.map Tuple.second extraFileContents

                                                        pkgModuleNames : Set String
                                                        pkgModuleNames =
                                                            allStableSources
                                                                |> List.filterMap DepGraph.parseModuleName
                                                                |> Set.fromList

                                                        rootModuleNames : Set String
                                                        rootModuleNames =
                                                            ProjectRoots.computeRootModuleNames
                                                                { allModuleNames = moduleGraph.moduleToSource |> Dict.keys |> Set.fromList
                                                                , pkgModuleNames = pkgModuleNames
                                                                , extraReachableImports = config.extraReachableImports
                                                                }

                                                        reachablePackageModuleNames : Set String
                                                        reachablePackageModuleNames =
                                                            reachableModules moduleGraph rootModuleNames
                                                                |> Set.intersect pkgModuleNames

                                                        allPackageSources : List String
                                                        allPackageSources =
                                                            topoSortModules moduleGraph reachablePackageModuleNames
                                                    in
                                                    Do.do BackendTask.Time.now <|
                                                        \graphFinish ->
                                                            let
                                                                buildGraphMs =
                                                                    stageMs graphStart graphFinish

                                                                packageSummaryCachePath : Maybe String
                                                                packageSummaryCachePath =
                                                                    config.packageParseCacheDir
                                                                        |> Maybe.map (\cacheDir -> packageSummaryCacheBlobPath cacheDir (packageSummaryCacheKey allPackageSources))

                                                                parseAndSeedPackageSummaries :
                                                                    { packageSummaryCacheHit : Int
                                                                    , loadPackageSummaryCacheMs : Int
                                                                    , decodePackageSummaryCacheMs : Int
                                                                    }
                                                                    ->
                                                                        BackendTask
                                                                            FatalError
                                                                            { packageSummaries : List CachedPackageModuleSummary
                                                                            , dependencySummaryStats : DependencySummaryStats
                                                                            , packageSummaryCacheHit : Int
                                                                            , packageSummaryCacheRoundtripOk : Int
                                                                            , packageSummaryCacheBytes : Int
                                                                            , loadPackageSummaryCacheMs : Int
                                                                            , decodePackageSummaryCacheMs : Int
                                                                            , validatePackageSummaryCacheMs : Int
                                                                            , parsePackageSourcesMs : Int
                                                                            , buildPackageSummariesFromParsedMs : Int
                                                                            , writePackageSummaryCacheMs : Int
                                                                            }
                                                                parseAndSeedPackageSummaries cacheMetrics =
                                                                    Do.do BackendTask.Time.now <|
                                                                        \parsePackageSourcesStart ->
                                                                            let
                                                                                parsedPackageSourcesResult =
                                                                                    Eval.Module.parseProjectSources allPackageSources
                                                                            in
                                                                            Do.do BackendTask.Time.now <|
                                                                                \parsePackageSourcesFinish ->
                                                                                    let
                                                                                        parsePackageSourcesMs =
                                                                                            stageMs parsePackageSourcesStart parsePackageSourcesFinish
                                                                                    in
                                                                                    case parsedPackageSourcesResult of
                                                                                        Err _ ->
                                                                                            BackendTask.fail (FatalError.fromString "Failed to build package environment")

                                                                                        Ok parsedPackageSources ->
                                                                                            Do.do BackendTask.Time.now <|
                                                                                                \buildPackageSummariesStart ->
                                                                                                    let
                                                                                                        normalizationResult =
                                                                                                            -- Normalize top-level constants here so the cache
                                                                                                            -- stores the rewritten bodies. Subsequent project
                                                                                                            -- loads hit the summary cache and skip both the
                                                                                                            -- normalization pass AND its re-evaluation cost.
                                                                                                            normalizeSummariesWithStats
                                                                                                                (Eval.Module.buildCachedModuleSummariesFromParsed parsedPackageSources)

                                                                                                        packageSummaries =
                                                                                                            normalizationResult.summaries
                                                                                                    in
                                                                                                    Do.do BackendTask.Time.now <|
                                                                                                        \buildPackageSummariesFinish ->
                                                                                                            let
                                                                                                                buildPackageSummariesFromParsedMs =
                                                                                                                    stageMs buildPackageSummariesStart buildPackageSummariesFinish
                                                                                                            in
                                                                                                            case packageSummaryCachePath of
                                                                                                                Just cachePath ->
                                                                                                                    let
                                                                                                                        cacheBytes =
                                                                                                                            encodePackageSummaryCache packageSummaries
                                                                                                                    in
                                                                                                                    Do.do BackendTask.Time.now <|
                                                                                                                        \validateCacheStart ->
                                                                                                                            let
                                                                                                                                cacheRoundtripOk =
                                                                                                                                    decodePackageSummaryCache cacheBytes /= Nothing
                                                                                                                            in
                                                                                                                            Do.do BackendTask.Time.now <|
                                                                                                                                \validateCacheFinish ->
                                                                                                                                    let
                                                                                                                                        baseInfo =
                                                                                                                                            { packageSummaries = packageSummaries
                                                                                                                                            , dependencySummaryStats = normalizationResult.stats
                                                                                                                                            , packageSummaryCacheHit = cacheMetrics.packageSummaryCacheHit
                                                                                                                                            , packageSummaryCacheRoundtripOk =
                                                                                                                                                if cacheRoundtripOk then
                                                                                                                                                    1

                                                                                                                                                else
                                                                                                                                                    0
                                                                                                                                            , packageSummaryCacheBytes = Bytes.width cacheBytes
                                                                                                                                            , loadPackageSummaryCacheMs = cacheMetrics.loadPackageSummaryCacheMs
                                                                                                                                            , decodePackageSummaryCacheMs = cacheMetrics.decodePackageSummaryCacheMs
                                                                                                                                            , validatePackageSummaryCacheMs = stageMs validateCacheStart validateCacheFinish
                                                                                                                                            , parsePackageSourcesMs = parsePackageSourcesMs
                                                                                                                                            , buildPackageSummariesFromParsedMs = buildPackageSummariesFromParsedMs
                                                                                                                                            , writePackageSummaryCacheMs = 0
                                                                                                                                            }
                                                                                                                                    in
                                                                                                                                    if cacheRoundtripOk then
                                                                                                                                        Do.do
                                                                                                                                            (withTiming
                                                                                                                                                (writeBinaryFile
                                                                                                                                                    { path = cachePath
                                                                                                                                                    , bytes = cacheBytes
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                        <|
                                                                                                                                            \writeCacheTimed ->
                                                                                                                                                BackendTask.succeed
                                                                                                                                                    { baseInfo | writePackageSummaryCacheMs = writeCacheTimed.ms }

                                                                                                                                    else
                                                                                                                                        BackendTask.succeed baseInfo

                                                                                                                Nothing ->
                                                                                                                    BackendTask.succeed
                                                                                                                        { packageSummaries = packageSummaries
                                                                                                                        , dependencySummaryStats = normalizationResult.stats
                                                                                                                        , packageSummaryCacheHit = cacheMetrics.packageSummaryCacheHit
                                                                                                                        , packageSummaryCacheRoundtripOk = 0
                                                                                                                        , packageSummaryCacheBytes = 0
                                                                                                                        , loadPackageSummaryCacheMs = cacheMetrics.loadPackageSummaryCacheMs
                                                                                                                        , decodePackageSummaryCacheMs = cacheMetrics.decodePackageSummaryCacheMs
                                                                                                                        , validatePackageSummaryCacheMs = 0
                                                                                                                        , parsePackageSourcesMs = parsePackageSourcesMs
                                                                                                                        , buildPackageSummariesFromParsedMs = buildPackageSummariesFromParsedMs
                                                                                                                        , writePackageSummaryCacheMs = 0
                                                                                                                        }
                                                            in
                                                            Do.do
                                                                (case packageSummaryCachePath of
                                                                    Just cachePath ->
                                                                        Do.do (File.exists cachePath |> BackendTask.allowFatal) <|
                                                                            \cacheExists ->
                                                                                if cacheExists then
                                                                                    Do.do (withTiming (File.binaryFile cachePath |> BackendTask.allowFatal)) <|
                                                                                        \readCacheTimed ->
                                                                                            Do.do BackendTask.Time.now <|
                                                                                                \decodeCacheStart ->
                                                                                                    let
                                                                                                        decodedCache =
                                                                                                            decodePackageSummaryCache readCacheTimed.value
                                                                                                    in
                                                                                                    Do.do BackendTask.Time.now <|
                                                                                                        \decodeCacheFinish ->
                                                                                                            case decodedCache of
                                                                                                                Just packageSummaries ->
                                                                                                                    BackendTask.succeed
                                                                                                                        { packageSummaries = packageSummaries
                                                                                                                        , dependencySummaryStats = emptyPackageSummaryStats
                                                                                                                        , packageSummaryCacheHit = 1
                                                                                                                        , packageSummaryCacheRoundtripOk = 1
                                                                                                                        , packageSummaryCacheBytes = Bytes.width readCacheTimed.value
                                                                                                                        , loadPackageSummaryCacheMs = readCacheTimed.ms
                                                                                                                        , decodePackageSummaryCacheMs = stageMs decodeCacheStart decodeCacheFinish
                                                                                                                        , validatePackageSummaryCacheMs = 0
                                                                                                                        , parsePackageSourcesMs = 0
                                                                                                                        , buildPackageSummariesFromParsedMs = 0
                                                                                                                        , writePackageSummaryCacheMs = 0
                                                                                                                        }

                                                                                                                Nothing ->
                                                                                                                    parseAndSeedPackageSummaries
                                                                                                                        { packageSummaryCacheHit = 0
                                                                                                                        , loadPackageSummaryCacheMs = readCacheTimed.ms
                                                                                                                        , decodePackageSummaryCacheMs = stageMs decodeCacheStart decodeCacheFinish
                                                                                                                        }

                                                                                else
                                                                                    parseAndSeedPackageSummaries
                                                                                        { packageSummaryCacheHit = 0
                                                                                        , loadPackageSummaryCacheMs = 0
                                                                                        , decodePackageSummaryCacheMs = 0
                                                                                        }

                                                                    Nothing ->
                                                                        parseAndSeedPackageSummaries
                                                                            { packageSummaryCacheHit = 0
                                                                            , loadPackageSummaryCacheMs = 0
                                                                            , decodePackageSummaryCacheMs = 0
                                                                            }
                                                                )
                                                            <|
                                                                \packageSummariesInfo ->
                                                                    let
                                                                        packageSummaries =
                                                                            packageSummariesInfo.packageSummaries
                                                                    in
                                                                    Do.do BackendTask.Time.now <|
                                                                        \buildPackageEnvStart ->
                                                                            let
                                                                                pkgEnvResult : Result Types.Error Eval.Module.ProjectEnv
                                                                                pkgEnvResult =
                                                                                    Eval.Module.buildProjectEnvFromSummaries packageSummaries
                                                                            in
                                                                            Do.do BackendTask.Time.now <|
                                                                                \buildPackageEnvFinish ->
                                                                                    let
                                                                                        buildPackageEnvFromSummariesMs =
                                                                                            stageMs buildPackageEnvStart buildPackageEnvFinish

                                                                                        buildPackageEnvMs =
                                                                                            packageSummariesInfo.parsePackageSourcesMs
                                                                                                + packageSummariesInfo.buildPackageSummariesFromParsedMs
                                                                                                + buildPackageEnvFromSummariesMs
                                                                                    in
                                                                                    case pkgEnvResult of
                                                                                        Err _ ->
                                                                                            BackendTask.fail (FatalError.fromString "Failed to build package environment")

                                                                                        Ok pkgEnv ->
                                                                                            Do.do BackendTask.Time.now <|
                                                                                                \baseUserEnvStart ->
                                                                                                    let
                                                                                                        userModuleNamesSet : Set String
                                                                                                        userModuleNamesSet =
                                                                                                            userParsedFiles |> Dict.keys |> Set.fromList

                                                                                                        userModulesInOrder : List File
                                                                                                        userModulesInOrder =
                                                                                                            topoSortModules moduleGraph userModuleNamesSet
                                                                                                                |> List.filterMap (\src -> DepGraph.parseModuleName src)
                                                                                                                |> List.filterMap (\name -> Dict.get name userParsedFiles)

                                                                                                        -- When `normalizationRoots` is provided, only normalize
                                                                                                        -- the user functions actually reachable from those
                                                                                                        -- roots. Module-level reachability is too coarse:
                                                                                                        -- one live function used to force normalization of
                                                                                                        -- every top-level in that module.
                                                                                                        normalizationTargetsByModule : Maybe (Dict String (Set String))
                                                                                                        normalizationTargetsByModule =
                                                                                                            case config.normalizationRoots of
                                                                                                                Nothing ->
                                                                                                                    Nothing

                                                                                                                Just rootModuleNamesList ->
                                                                                                                    let
                                                                                                                        seeds : Set FunctionReachability.Reference
                                                                                                                        seeds =
                                                                                                                            rootModuleNamesList
                                                                                                                                |> List.concatMap
                                                                                                                                    (\rmn ->
                                                                                                                                        case Dict.get rmn userParsedFiles of
                                                                                                                                            Just file ->
                                                                                                                                                FunctionReachability.findTopLevelNames file
                                                                                                                                                    |> Set.toList
                                                                                                                                                    |> List.map (\name -> ( rmn, name ))

                                                                                                                                            Nothing ->
                                                                                                                                                []
                                                                                                                                    )
                                                                                                                                |> Set.fromList

                                                                                                                        reachableFns : Set FunctionReachability.Reference
                                                                                                                        reachableFns =
                                                                                                                            FunctionReachability.computeReachable userParsedFiles seeds

                                                                                                                        reachableByModule : Dict String (Set String)
                                                                                                                        reachableByModule =
                                                                                                                            reachableFns
                                                                                                                                |> Set.foldl
                                                                                                                                    (\( moduleName, functionName ) acc ->
                                                                                                                                        Dict.update moduleName
                                                                                                                                            (\maybeFunctions ->
                                                                                                                                                Just
                                                                                                                                                    (Set.insert functionName
                                                                                                                                                        (Maybe.withDefault Set.empty maybeFunctions)
                                                                                                                                                    )
                                                                                                                                            )
                                                                                                                                            acc
                                                                                                                                    )
                                                                                                                                    Dict.empty
                                                                                                                    in
                                                                                                                    Just reachableByModule

                                                                                                        userModulePlans : List UserNormModulePlan
                                                                                                        userModulePlans =
                                                                                                            case normalizationTargetsByModule of
                                                                                                                Nothing ->
                                                                                                                    userModulesInOrder
                                                                                                                        |> List.map
                                                                                                                            (\file ->
                                                                                                                                { file = file
                                                                                                                                , targetFunctions = Nothing
                                                                                                                                }
                                                                                                                            )

                                                                                                                Just reachableByModule ->
                                                                                                                    userModulesInOrder
                                                                                                                        |> List.filterMap
                                                                                                                            (\file ->
                                                                                                                               case Eval.Module.fileModuleName file of
                                                                                                                                   name ->
                                                                                                                                        Dict.get (String.join "." name) reachableByModule
                                                                                                                                            |> Maybe.map
                                                                                                                                                (\targetFunctions ->
                                                                                                                                                    { file = file
                                                                                                                                                    , targetFunctions = Just targetFunctions
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                            )

                                                                                                        envBeforeUserNormResult : Result Types.Error Eval.Module.ProjectEnv
                                                                                                        envBeforeUserNormResult =
                                                                                                            Eval.Module.extendWithFiles pkgEnv userModulesInOrder
                                                                                                    in
                                                                                                    Do.do
                                                                                                        (case envBeforeUserNormResult of
                                                                                                            Err _ ->
                                                                                                                BackendTask.succeed Nothing

                                                                                                            Ok envBeforeUserNorm ->
                                                                                                                buildUserNormalizedEnv
                                                                                                                    { cacheDir = config.packageParseCacheDir
                                                                                                                    , packageKey = packageSummaryCacheKey allPackageSources
                                                                                                                    , userModulePlans = userModulePlans
                                                                                                                    , userNormalizationFlags = userNormalizationFlags
                                                                                                                    , userFileContents =
                                                                                                                        userFileContents
                                                                                                                            |> List.filterMap
                                                                                                                               (\( _, content ) ->
                                                                                                                                    DepGraph.parseModuleName content
                                                                                                                                        |> Maybe.map (\name -> ( name, content ))
                                                                                                                                )
                                                                                                                            |> Dict.fromList
                                                                                                                    }
                                                                                                                    envBeforeUserNorm
                                                                                                                    |> BackendTask.map
                                                                                                                        (\normalizedUserEnv ->
                                                                                                                            -- Pre-resolve user modules into the resolved-IR's
                                                                                                                            -- `bodies` map. Without this, the resolved-IR
                                                                                                                            -- evaluator's `dispatchGlobalApplyStep` misses
                                                                                                                            -- on user functions and falls back to the old
                                                                                                                            -- string-keyed evaluator on every call — which
                                                                                                                            -- dominates the cold-eval profile for
                                                                                                                            -- fuzz-heavy test suites.
                                                                                                                            Just
                                                                                                                                { env =
                                                                                                                                    Eval.Module.extendResolvedWithFiles userModulesInOrder normalizedUserEnv.env
                                                                                                                                , stats = normalizedUserEnv.stats
                                                                                                                                }
                                                                                                                        )
                                                                                                        )
                                                                                                    <|
                                                                                                        \baseUserEnvResult ->
                                                                                                            Do.do BackendTask.Time.now <|
                                                                                                                \baseUserEnvFinish ->
                                                                                                                    let
                                                                                                                        buildBaseUserEnvMs =
                                                                                                                            stageMs baseUserEnvStart baseUserEnvFinish
                                                                                                                    in
                                                                                                                    Do.do BackendTask.Time.now <|
                                                                                                                        \semanticIndexStart ->
                                                                                                                            let
                                                                                                                                semanticIndex =
                                                                                                                                    userFileContents
                                                                                                                                        |> List.map
                                                                                                                                            (\( filePath, content ) ->
                                                                                                                                                { moduleName =
                                                                                                                                                    DepGraph.parseModuleName content
                                                                                                                                                        |> Maybe.withDefault filePath
                                                                                                                                                , source = content
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        |> SemanticHash.buildMultiModuleIndex
                                                                                                                            in
                                                                                                                            Do.do BackendTask.Time.now <|
                                                                                                                                \semanticIndexFinish ->
                                                                                                                                    let
                                                                                                                                        buildSemanticIndexMs =
                                                                                                                                            stageMs semanticIndexStart semanticIndexFinish
                                                                                                                                    in
                                                                                                                                    Do.do (withTiming (Cache.inputs allUserPaths)) <|
                                                                                                                                        \sourceInputsTimed ->
                                                                                                                                            BackendTask.succeed
                                                                                                                                                { project =
                                                                                                                                                    InterpreterProject
                                                                                                                                                        { sourceDirectories = sourceDirectories
                                                                                                                                                        , inputsByPath =
                                                                                                                                                            sourceInputsTimed.value
                                                                                                                                                                |> List.map
                                                                                                                                                                    (\( pathVal, monad ) ->
                                                                                                                                                                        ( Path.toString pathVal
                                                                                                                                                                        , ( pathVal, monad )
                                                                                                                                                                        )
                                                                                                                                                                    )
                                                                                                                                                                |> Dict.fromList
                                                                                                                                                        , userFileContents =
                                                                                                                                                            userFileContents
                                                                                                                                                                |> Dict.fromList
                                                                                                                                                        , depGraph = depGraph
                                                                                                                                                        , patchedPackageSources = patchedPackageSources
                                                                                                                                                        , extraSources =
                                                                                                                                                            extraFileContents
                                                                                                                                                                |> List.map Tuple.second
                                                                                                                                                        , moduleGraph = moduleGraph
                                                                                                                                                        , packageModuleNames = pkgModuleNames
                                                                                                                                                        , packageEnv = pkgEnv
                                                                                                                                                        , baseUserEnv = baseUserEnvResult |> Maybe.map .env
                                                                                                                                                        , semanticIndex = semanticIndex
                                                                                                                                                        , envMode = LegacyAst
                                                                                                                                                        }
                                                                                                                                                , profile =
                                                                                                                                                    let
                                                                                                                                                        userNormStats =
                                                                                                                                                            baseUserEnvResult
                                                                                                                                                                |> Maybe.map .stats
                                                                                                                                                                |> Maybe.withDefault emptyUserNormStats

                                                                                                                                                        packageResolvedEnv =
                                                                                                                                                            Eval.Module.projectEnvResolved pkgEnv

                                                                                                                                                        finalResolvedEnv =
                                                                                                                                                            baseUserEnvResult
                                                                                                                                                                |> Maybe.map (.env >> Eval.Module.projectEnvResolved)
                                                                                                                                                                |> Maybe.withDefault packageResolvedEnv

                                                                                                                                                        packageResolvedErrorSummary =
                                                                                                                                                            summarizeResolveErrors packageResolvedEnv.errors

                                                                                                                                                        finalResolvedErrorSummary =
                                                                                                                                                            summarizeResolveErrors finalResolvedEnv.errors
                                                                                                                                                    in
                                                                                                                                                    { resolveSourceDirectoriesMs = sourceDirectoriesTimed.ms
                                                                                                                                                    , loadPackageSourcesMs = packageSourcesTimed.ms
                                                                                                                                                    , globUserSourcesMs = elmFilesTimed.ms
                                                                                                                                                    , readUserSourcesMs = userFileContentsTimed.ms
                                                                                                                                                    , readExtraSourcesMs = extraFileContentsTimed.ms
                                                                                                                                                    , packageSummaryCacheHit = packageSummariesInfo.packageSummaryCacheHit
                                                                                                                                                    , packageSummaryCacheRoundtripOk = packageSummariesInfo.packageSummaryCacheRoundtripOk
                                                                                                                                                    , packageSummaryCacheBytes = packageSummariesInfo.packageSummaryCacheBytes
                                                                                                                                                    , loadPackageSummaryCacheMs = packageSummariesInfo.loadPackageSummaryCacheMs
                                                                                                                                                    , decodePackageSummaryCacheMs = packageSummariesInfo.decodePackageSummaryCacheMs
                                                                                                                                                    , validatePackageSummaryCacheMs = packageSummariesInfo.validatePackageSummaryCacheMs
                                                                                                                                                    , writePackageSummaryCacheMs = packageSummariesInfo.writePackageSummaryCacheMs
                                                                                                                                                    , buildGraphMs = buildGraphMs
                                                                                                                                                    , parsePackageSourcesMs = packageSummariesInfo.parsePackageSourcesMs
                                                                                                                                                    , buildPackageSummariesFromParsedMs = packageSummariesInfo.buildPackageSummariesFromParsedMs
                                                                                                                                                    , packageSummaryFunctionsVisited = packageSummariesInfo.dependencySummaryStats.functionsVisited
                                                                                                                                                    , packageSummaryFunctionsRewritten = packageSummariesInfo.dependencySummaryStats.functionsRewritten
                                                                                                                                                    , packageSummaryInlineCandidates = packageSummariesInfo.dependencySummaryStats.inlineCandidates
                                                                                                                                                    , packageSummaryInlineSuccesses = packageSummariesInfo.dependencySummaryStats.inlineSuccesses
                                                                                                                                                    , packageSummaryInlineRejectedPattern = packageSummariesInfo.dependencySummaryStats.inlineRejectedPattern
                                                                                                                                                    , packageSummaryInlineRejectedArity = packageSummariesInfo.dependencySummaryStats.inlineRejectedArity
                                                                                                                                                    , packageSummaryInlineRejectedSelfCall = packageSummariesInfo.dependencySummaryStats.inlineRejectedSelfCall
                                                                                                                                                    , packageSummaryInlineRejectedBodyTooLarge = packageSummariesInfo.dependencySummaryStats.inlineRejectedBodyTooLarge
                                                                                                                                                    , packageSummaryInlineRejectedUnsafe = packageSummariesInfo.dependencySummaryStats.inlineRejectedUnsafe
                                                                                                                                                    , packageSummaryInlineRejectedUnsafeApplication = packageSummariesInfo.dependencySummaryStats.inlineRejectedUnsafeApplication
                                                                                                                                                    , packageSummaryInlineRejectedUnsafeIf = packageSummariesInfo.dependencySummaryStats.inlineRejectedUnsafeIf
                                                                                                                                                    , packageSummaryInlineRejectedUnsafeCase = packageSummariesInfo.dependencySummaryStats.inlineRejectedUnsafeCase
                                                                                                                                                    , packageSummaryInlineRejectedUnsafeLet = packageSummariesInfo.dependencySummaryStats.inlineRejectedUnsafeLet
                                                                                                                                                    , packageSummaryInlineRejectedUnsafeLambda = packageSummariesInfo.dependencySummaryStats.inlineRejectedUnsafeLambda
                                                                                                                                                    , packageSummaryInlineRejectedUnsafeOther = packageSummariesInfo.dependencySummaryStats.inlineRejectedUnsafeOther
                                                                                                                                                    , packageSummaryInlineRejectedInternalHelper = packageSummariesInfo.dependencySummaryStats.inlineRejectedInternalHelper
                                                                                                                                                    , packageSummaryInlineBodyLt30 = packageSummariesInfo.dependencySummaryStats.inlineBodyLt30
                                                                                                                                                    , packageSummaryInlineBody30To59 = packageSummariesInfo.dependencySummaryStats.inlineBody30To59
                                                                                                                                                    , packageSummaryInlineBody60Plus = packageSummariesInfo.dependencySummaryStats.inlineBody60Plus
                                                                                                                                                    , packageSummaryInlineShapeLeaf = packageSummariesInfo.dependencySummaryStats.inlineShapeLeaf
                                                                                                                                                    , packageSummaryInlineShapeConstructor = packageSummariesInfo.dependencySummaryStats.inlineShapeConstructor
                                                                                                                                                    , packageSummaryInlineShapeOperator = packageSummariesInfo.dependencySummaryStats.inlineShapeOperator
                                                                                                                                                    , packageSummaryInlineShapeRecordAccess = packageSummariesInfo.dependencySummaryStats.inlineShapeRecordAccess
                                                                                                                                                    , packageSummaryInlineShapeCollection = packageSummariesInfo.dependencySummaryStats.inlineShapeCollection
                                                                                                                                                    , packageSummaryInlineShapeOther = packageSummariesInfo.dependencySummaryStats.inlineShapeOther
                                                                                                                                                    , packageSummaryInlinePayoffChanged = packageSummariesInfo.dependencySummaryStats.inlinePayoffChanged
                                                                                                                                                    , packageSummaryInlinePayoffChangedShapeLeaf = packageSummariesInfo.dependencySummaryStats.inlinePayoffChangedShapeLeaf
                                                                                                                                                    , packageSummaryInlinePayoffChangedShapeConstructor = packageSummariesInfo.dependencySummaryStats.inlinePayoffChangedShapeConstructor
                                                                                                                                                    , packageSummaryInlinePayoffChangedShapeOperator = packageSummariesInfo.dependencySummaryStats.inlinePayoffChangedShapeOperator
                                                                                                                                                    , packageSummaryInlinePayoffChangedShapeRecordAccess = packageSummariesInfo.dependencySummaryStats.inlinePayoffChangedShapeRecordAccess
                                                                                                                                                    , packageSummaryInlinePayoffChangedShapeCollection = packageSummariesInfo.dependencySummaryStats.inlinePayoffChangedShapeCollection
                                                                                                                                                    , packageSummaryInlinePayoffChangedShapeOther = packageSummariesInfo.dependencySummaryStats.inlinePayoffChangedShapeOther
                                                                                                                                                    , packageSummaryInlinePayoffChangedBodyLt30 = packageSummariesInfo.dependencySummaryStats.inlinePayoffChangedBodyLt30
                                                                                                                                                    , packageSummaryInlinePayoffChangedBody30To59 = packageSummariesInfo.dependencySummaryStats.inlinePayoffChangedBody30To59
                                                                                                                                                    , packageSummaryInlinePayoffChangedBody60Plus = packageSummariesInfo.dependencySummaryStats.inlinePayoffChangedBody60Plus
                                                                                                                                                    , packageSummaryInlinePayoffInline = packageSummariesInfo.dependencySummaryStats.inlinePayoffInline
                                                                                                                                                    , packageSummaryInlinePayoffInlineShapeLeaf = packageSummariesInfo.dependencySummaryStats.inlinePayoffInlineShapeLeaf
                                                                                                                                                    , packageSummaryInlinePayoffInlineShapeConstructor = packageSummariesInfo.dependencySummaryStats.inlinePayoffInlineShapeConstructor
                                                                                                                                                    , packageSummaryInlinePayoffInlineShapeOperator = packageSummariesInfo.dependencySummaryStats.inlinePayoffInlineShapeOperator
                                                                                                                                                    , packageSummaryInlinePayoffInlineShapeRecordAccess = packageSummariesInfo.dependencySummaryStats.inlinePayoffInlineShapeRecordAccess
                                                                                                                                                    , packageSummaryInlinePayoffInlineShapeCollection = packageSummariesInfo.dependencySummaryStats.inlinePayoffInlineShapeCollection
                                                                                                                                                    , packageSummaryInlinePayoffInlineShapeOther = packageSummariesInfo.dependencySummaryStats.inlinePayoffInlineShapeOther
                                                                                                                                                    , packageSummaryInlinePayoffInlineBodyLt30 = packageSummariesInfo.dependencySummaryStats.inlinePayoffInlineBodyLt30
                                                                                                                                                    , packageSummaryInlinePayoffInlineBody30To59 = packageSummariesInfo.dependencySummaryStats.inlinePayoffInlineBody30To59
                                                                                                                                                    , packageSummaryInlinePayoffInlineBody60Plus = packageSummariesInfo.dependencySummaryStats.inlinePayoffInlineBody60Plus
                                                                                                                                                    , packageSummaryInlinePayoffConstantFold = packageSummariesInfo.dependencySummaryStats.inlinePayoffConstantFold
                                                                                                                                                    , packageSummaryInlinePayoffConstantFoldShapeLeaf = packageSummariesInfo.dependencySummaryStats.inlinePayoffConstantFoldShapeLeaf
                                                                                                                                                    , packageSummaryInlinePayoffConstantFoldShapeConstructor = packageSummariesInfo.dependencySummaryStats.inlinePayoffConstantFoldShapeConstructor
                                                                                                                                                    , packageSummaryInlinePayoffConstantFoldShapeOperator = packageSummariesInfo.dependencySummaryStats.inlinePayoffConstantFoldShapeOperator
                                                                                                                                                    , packageSummaryInlinePayoffConstantFoldShapeRecordAccess = packageSummariesInfo.dependencySummaryStats.inlinePayoffConstantFoldShapeRecordAccess
                                                                                                                                                    , packageSummaryInlinePayoffConstantFoldShapeCollection = packageSummariesInfo.dependencySummaryStats.inlinePayoffConstantFoldShapeCollection
                                                                                                                                                    , packageSummaryInlinePayoffConstantFoldShapeOther = packageSummariesInfo.dependencySummaryStats.inlinePayoffConstantFoldShapeOther
                                                                                                                                                    , packageSummaryInlinePayoffConstantFoldBodyLt30 = packageSummariesInfo.dependencySummaryStats.inlinePayoffConstantFoldBodyLt30
                                                                                                                                                    , packageSummaryInlinePayoffConstantFoldBody30To59 = packageSummariesInfo.dependencySummaryStats.inlinePayoffConstantFoldBody30To59
                                                                                                                                                    , packageSummaryInlinePayoffConstantFoldBody60Plus = packageSummariesInfo.dependencySummaryStats.inlinePayoffConstantFoldBody60Plus
                                                                                                                                                    , packageSummaryInlinePayoffPrecomputedRef = packageSummariesInfo.dependencySummaryStats.inlinePayoffPrecomputedRef
                                                                                                                                                    , packageSummaryInlinePayoffPrecomputedRefShapeLeaf = packageSummariesInfo.dependencySummaryStats.inlinePayoffPrecomputedRefShapeLeaf
                                                                                                                                                    , packageSummaryInlinePayoffPrecomputedRefShapeConstructor = packageSummariesInfo.dependencySummaryStats.inlinePayoffPrecomputedRefShapeConstructor
                                                                                                                                                    , packageSummaryInlinePayoffPrecomputedRefShapeOperator = packageSummariesInfo.dependencySummaryStats.inlinePayoffPrecomputedRefShapeOperator
                                                                                                                                                    , packageSummaryInlinePayoffPrecomputedRefShapeRecordAccess = packageSummariesInfo.dependencySummaryStats.inlinePayoffPrecomputedRefShapeRecordAccess
                                                                                                                                                    , packageSummaryInlinePayoffPrecomputedRefShapeCollection = packageSummariesInfo.dependencySummaryStats.inlinePayoffPrecomputedRefShapeCollection
                                                                                                                                                    , packageSummaryInlinePayoffPrecomputedRefShapeOther = packageSummariesInfo.dependencySummaryStats.inlinePayoffPrecomputedRefShapeOther
                                                                                                                                                    , packageSummaryInlinePayoffPrecomputedRefBodyLt30 = packageSummariesInfo.dependencySummaryStats.inlinePayoffPrecomputedRefBodyLt30
                                                                                                                                                    , packageSummaryInlinePayoffPrecomputedRefBody30To59 = packageSummariesInfo.dependencySummaryStats.inlinePayoffPrecomputedRefBody30To59
                                                                                                                                                    , packageSummaryInlinePayoffPrecomputedRefBody60Plus = packageSummariesInfo.dependencySummaryStats.inlinePayoffPrecomputedRefBody60Plus
                                                                                                                                                    , packageSummaryInlineShadowRejectCollection = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectCollection
                                                                                                                                                    , packageSummaryInlineShadowRejectCollectionPayoffChanged = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectCollectionPayoffChanged
                                                                                                                                                    , packageSummaryInlineShadowRejectCollectionPayoffInline = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectCollectionPayoffInline
                                                                                                                                                    , packageSummaryInlineShadowRejectCollectionPayoffPrecomputedRef = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectCollectionPayoffPrecomputedRef
                                                                                                                                                    , packageSummaryInlineShadowRejectCollectionFinalShrinks = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectCollectionFinalShrinks
                                                                                                                                                    , packageSummaryInlineShadowRejectCollectionFinalNonApplication = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectCollectionFinalNonApplication
                                                                                                                                                    , packageSummaryInlineShadowRejectCollectionFinalDirectRootWin = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectCollectionFinalDirectRootWin
                                                                                                                                                    , packageSummaryInlineShadowRejectCollectionFinalConstructorApplication = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectCollectionFinalConstructorApplication
                                                                                                                                                    , packageSummaryInlineShadowRejectCollectionNoPayoffNoDirectBenefit = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectCollectionNoPayoffNoDirectBenefit
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth0 = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth0
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth0PayoffChanged = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth0PayoffChanged
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth0PayoffInline = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth0PayoffInline
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth0PayoffPrecomputedRef = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth0PayoffPrecomputedRef
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth0FinalShrinks = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth0FinalShrinks
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth0FinalNonApplication = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth0FinalNonApplication
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth0FinalDirectRootWin = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth0FinalDirectRootWin
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth0FinalConstructorApplication = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth0FinalConstructorApplication
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth0NoPayoffNoDirectBenefit = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth0NoPayoffNoDirectBenefit
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth1 = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth1
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth1PayoffChanged = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth1PayoffChanged
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth1PayoffInline = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth1PayoffInline
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth1PayoffPrecomputedRef = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth1PayoffPrecomputedRef
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth1FinalShrinks = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth1FinalShrinks
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth1FinalNonApplication = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth1FinalNonApplication
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth1FinalDirectRootWin = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth1FinalDirectRootWin
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth1FinalConstructorApplication = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth1FinalConstructorApplication
                                                                                                                                                    , packageSummaryInlineShadowRejectGrowth1NoPayoffNoDirectBenefit = packageSummariesInfo.dependencySummaryStats.inlineShadowRejectGrowth1NoPayoffNoDirectBenefit
                                                                                                                                                    , packageSummaryListFusionChanges = packageSummariesInfo.dependencySummaryStats.listFusionChanges
                                                                                                                                                    , packageSummaryListFusionPipelineNormalizations = packageSummariesInfo.dependencySummaryStats.listFusionPipelineNormalizations
                                                                                                                                                    , packageSummaryListFusionHeadFlattenRewrites = packageSummariesInfo.dependencySummaryStats.listFusionHeadFlattenRewrites
                                                                                                                                                    , packageSummaryListFusionRuleRewrites = packageSummariesInfo.dependencySummaryStats.listFusionRuleRewrites
                                                                                                                                                    , packageSummaryPrecomputedRefSubstitutions = packageSummariesInfo.dependencySummaryStats.precomputedRefSubstitutions
                                                                                                                                                    , packageSummaryConstantFolds = packageSummariesInfo.dependencySummaryStats.constantFolds
                                                                                                                                                    , packageSummaryRejectSamples = packageSummariesInfo.dependencySummaryStats.rejectSamples
                                                                                                                                                    , buildPackageEnvFromSummariesMs = buildPackageEnvFromSummariesMs
                                                                                                                                                    , buildPackageEnvMs = buildPackageEnvMs
                                                                                                                                                    , packageResolvedErrorsCount = List.length packageResolvedEnv.errors
                                                                                                                                                    , packageResolvedBodiesCount = FastDict.size packageResolvedEnv.bodies
                                                                                                                                                    , packageResolvedGlobalsCount = FastDict.size packageResolvedEnv.globals
                                                                                                                                                    , packageResolvedErrorSummary = packageResolvedErrorSummary
                                                                                                                                                    , resolvedErrorsCount = List.length finalResolvedEnv.errors
                                                                                                                                                    , resolvedBodiesCount = FastDict.size finalResolvedEnv.bodies
                                                                                                                                                    , resolvedGlobalsCount = FastDict.size finalResolvedEnv.globals
                                                                                                                                                    , resolvedErrorSummary = finalResolvedErrorSummary
                                                                                                                                                    , buildBaseUserEnvMs = buildBaseUserEnvMs
                                                                                                                                                    , userNormModulesPlanned = userNormStats.modulesPlanned
                                                                                                                                                    , userNormTargetFunctions = userNormStats.targetFunctions
                                                                                                                                                    , userNormCacheHitModules = userNormStats.cacheHitModules
                                                                                                                                                    , userNormCacheMissModules = userNormStats.cacheMissModules
                                                                                                                                                    , userNormCacheExtendedModules = userNormStats.cacheExtendedModules
                                                                                                                                                    , userNormRewrittenFunctions = userNormStats.rewrittenFunctions
                                                                                                                                                    , userNormPrecomputedValues = userNormStats.precomputedValues
                                                                                                                                                    , userNormDependencySummaryStats = userNormStats.dependencySummaryStats
                                                                                                                                                    , buildSemanticIndexMs = buildSemanticIndexMs
                                                                                                                                                    , cacheInputsMs = sourceInputsTimed.ms
                                                                                                                                                    }
                                                                                                                                                }


{-| Read the "source-directories" field from elm.json in the given project directory.
-}
readSourceDirectories : Path -> BackendTask FatalError (List String)
readSourceDirectories projectDir =
    File.rawFile (Path.toString projectDir ++ "/elm.json")
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\raw ->
                case Decode.decodeString (Decode.field "source-directories" (Decode.list Decode.string)) raw of
                    Ok dirs ->
                        BackendTask.succeed dirs

                    Err err ->
                        BackendTask.fail (FatalError.fromString ("Failed to decode source-directories from elm.json: " ++ Decode.errorToString err))
            )


{-| Evaluate a `"ModuleName.valueName"` expression and cache the result.

Parses the expression to determine the module name, then delegates to
`evalWith` with that single import.

-}
eval : InterpreterProject -> String -> (FileOrDirectory -> Cache.Monad a) -> Cache.Monad a
eval project expression k =
    case parseExpression expression of
        Just ( moduleName, _ ) ->
            evalWith project { imports = [ moduleName ], expression = expression } k

        Nothing ->
            Cache.fail ("Invalid expression: " ++ expression ++ " (expected \"ModuleName.valueName\")")


{-| Evaluate an arbitrary Elm expression with multiple imports via the interpreter.

The expression can reference any of the imported modules and must produce
a `String`. Transitive dependencies are computed as the union across all
imported modules. The result is cached based on the combined hash of
package sources, extra sources, relevant user sources, and the wrapper module.

Package modules are parsed once during `loadWith` and reused across all
`evalWith` calls. Only user-specific sources are parsed per call.

-}
evalWith : InterpreterProject -> { imports : List String, expression : String } -> (FileOrDirectory -> Cache.Monad a) -> Cache.Monad a
evalWith (InterpreterProject project) { imports, expression } k =
    let
        -- Get transitive user-source deps as the union across all imported modules
        transDeps : Set String
        transDeps =
            imports
                |> List.filterMap (DepGraph.moduleNameToFilePath project.depGraph)
                |> List.map (DepGraph.transitiveDeps project.depGraph)
                |> List.foldl Set.union Set.empty

        -- Filter inputsByPath to relevant source deps
        relevantInputs : List ( String, Cache.Monad FileOrDirectory )
        relevantInputs =
            project.inputsByPath
                |> Dict.toList
                |> List.filter
                    (\( path, _ ) ->
                        Set.member path transDeps
                    )
                |> List.map (\( path, ( _, monad ) ) -> ( path, monad ))

        -- Generate wrapper module
        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        -- Compute transitive module deps from the wrapper's imports through the module graph
        wrapperImports : Set String
        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules : Set String
        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        -- Topologically sort needed modules and collect their sources
        filteredSources : List String
        filteredSources =
            topoSortModules project.moduleGraph neededModules

        -- Filter to only non-package (user) sources
        userFilteredSources : List String
        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )

        -- All sources for hashing (includes everything for correct cache keys)
        allFilteredSources : List String
        allFilteredSources =
            filteredSources ++ [ wrapperSource ]

        -- Single blob of all filtered sources for hashing
        filteredBlob : String
        filteredBlob =
            String.join "\n---MODULE_SEPARATOR---\n" allFilteredSources
    in
    -- Hash the filtered blob
    Cache.do (Cache.writeFile filteredBlob Cache.succeed) <|
        \filteredHash ->
            -- Resolve all relevant user file hashes
            Cache.do
                (relevantInputs
                    |> List.map
                        (\( path, monad ) ->
                            Cache.do monad <|
                                \hash ->
                                    Cache.succeed { filename = Path.path path, hash = hash }
                        )
                    |> Cache.sequence
                )
            <|
                \sourceFiles ->
                    -- Combine all hashes into a single combined hash
                    Cache.do
                        (Cache.combine
                            (sourceFiles
                                ++ [ { filename = Path.path "filtered.blob", hash = filteredHash }
                                   ]
                            )
                        )
                    <|
                        \combinedHash ->
                            -- Cache the interpreter computation
                            Cache.compute [ "interpret", interpreterResultCacheVersion ]
                                combinedHash
                                (\() ->
                                    let
                                        -- Only parse user sources + wrapper; reuse the pre-built package env
                                        result : Result Types.Error Types.Value
                                        result =
                                            Eval.Module.evalWithEnv
                                                project.packageEnv
                                                (userFilteredSources ++ [ wrapperSource ])
                                                (FunctionOrValue [] "results")
                                    in
                                    case result of
                                        Ok (Types.String s) ->
                                            s

                                        Ok other ->
                                            "ERROR: Expected String result"

                                        Err (Types.ParsingError _) ->
                                            "ERROR: Parsing error"

                                        Err (Types.EvalError evalErr) ->
                                            "ERROR: Eval error: "
                                                ++ evalErrorKindToString evalErr.error
                                                ++ " [module: "
                                                ++ String.join "." evalErr.currentModule
                                                ++ "] [stack: "
                                                ++ (evalErr.callStack |> List.take 10 |> List.map (\ref -> String.join "." ref.moduleName ++ "." ++ ref.name) |> String.join " <- ")
                                                ++ "]"
                                )
                                k


{-| Evaluate an expression with source overrides.

Like `evalWith`, but allows overriding specific module sources before
evaluation. Override sources take precedence over existing modules with
the same name. Useful for mutation testing, where you want to evaluate
the same expression with a modified version of one module.

The cache key includes the override source hashes, so each unique
mutation gets its own cached result.

-}
evalWithSourceOverrides : InterpreterProject -> { imports : List String, expression : String, sourceOverrides : List String } -> (FileOrDirectory -> Cache.Monad a) -> Cache.Monad a
evalWithSourceOverrides (InterpreterProject project) { imports, expression, sourceOverrides } k =
    let
        transDeps : Set String
        transDeps =
            imports
                |> List.filterMap (DepGraph.moduleNameToFilePath project.depGraph)
                |> List.map (DepGraph.transitiveDeps project.depGraph)
                |> List.foldl Set.union Set.empty

        relevantInputs : List ( String, Cache.Monad FileOrDirectory )
        relevantInputs =
            project.inputsByPath
                |> Dict.toList
                |> List.filter (\( path, _ ) -> Set.member path transDeps)
                |> List.map (\( path, ( _, monad ) ) -> ( path, monad ))

        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        wrapperImports : Set String
        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules : Set String
        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        filteredSources : List String
        filteredSources =
            topoSortModules project.moduleGraph neededModules

        userFilteredSources : List String
        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )

        -- Include overrides in the content blob for hashing
        allFilteredSources : List String
        allFilteredSources =
            filteredSources ++ sourceOverrides ++ [ wrapperSource ]

        filteredBlob : String
        filteredBlob =
            String.join "\n---MODULE_SEPARATOR---\n" allFilteredSources
    in
    Cache.do (Cache.writeFile filteredBlob Cache.succeed) <|
        \filteredHash ->
            Cache.do
                (relevantInputs
                    |> List.map
                        (\( path, monad ) ->
                            Cache.do monad <|
                                \hash ->
                                    Cache.succeed { filename = Path.path path, hash = hash }
                        )
                    |> Cache.sequence
                )
            <|
                \sourceFiles ->
                    Cache.do
                        (Cache.combine
                            (sourceFiles
                                ++ [ { filename = Path.path "filtered.blob", hash = filteredHash } ]
                            )
                        )
                    <|
                        \combinedHash ->
                            Cache.compute [ "interpret-with-overrides", interpreterResultCacheVersion ]
                                combinedHash
                                (\() ->
                                    let
                                        -- User sources first, then overrides (which replace same-named
                                        -- modules since evalWithEnv processes in order, last wins).
                                        -- Wrapper goes last for import resolution.
                                        result : Result Types.Error Types.Value
                                        result =
                                            Eval.Module.evalWithEnv
                                                project.packageEnv
                                                (userFilteredSources ++ sourceOverrides ++ [ wrapperSource ])
                                                (FunctionOrValue [] "results")
                                    in
                                    case result of
                                        Ok (Types.String s) ->
                                            s

                                        Ok _ ->
                                            "ERROR: Expected String result"

                                        Err (Types.ParsingError _) ->
                                            "ERROR: Parsing error"

                                        Err (Types.EvalError evalErr) ->
                                            "ERROR: Eval error: "
                                                ++ evalErrorKindToString evalErr.error
                                                ++ " [module: "
                                                ++ String.join "." evalErr.currentModule
                                                ++ "] [stack: "
                                                ++ (evalErr.callStack |> List.take 10 |> List.map (\ref -> String.join "." ref.moduleName ++ "." ++ ref.name) |> String.join " <- ")
                                                ++ "]"
                                )
                                k


{-| Evaluate an expression with pre-parsed File AST overrides.

Like `evalWithSourceOverrides`, but accepts `File` ASTs directly instead of
source strings for the overrides. Skips the write→parse round-trip for mutations.

The `sourceOverrides` are the string sources that still need parsing (e.g. SimpleTestRunner).
The `fileOverrides` are pre-parsed ASTs (e.g. mutated File from Mutator).

-}
evalWithFileOverrides :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , fileOverrides : List { file : File, hashKey : String }
        }
    -> (FileOrDirectory -> Cache.Monad a)
    -> Cache.Monad a
evalWithFileOverrides (InterpreterProject project) { imports, expression, sourceOverrides, fileOverrides } k =
    let
        transDeps : Set String
        transDeps =
            imports
                |> List.filterMap (DepGraph.moduleNameToFilePath project.depGraph)
                |> List.map (DepGraph.transitiveDeps project.depGraph)
                |> List.foldl Set.union Set.empty

        relevantInputs : List ( String, Cache.Monad FileOrDirectory )
        relevantInputs =
            project.inputsByPath
                |> Dict.toList
                |> List.filter (\( path, _ ) -> Set.member path transDeps)
                |> List.map (\( path, ( _, monad ) ) -> ( path, monad ))

        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        wrapperImports : Set String
        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules : Set String
        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        filteredSources : List String
        filteredSources =
            topoSortModules project.moduleGraph neededModules

        userFilteredSources : List String
        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )

        -- Pre-parsed user module Files (from loadWith), in topo order
        userFilteredFiles : List File
        userFilteredFiles =
            neededModules
                |> Set.toList
                |> List.filter (\name -> not (Set.member name project.packageModuleNames))
                |> List.filterMap (\name -> Dict.get name project.moduleGraph.moduleToFile)

        -- Semantic hash: hash the wrapper expression's semantic dependencies
        -- + override hash keys. Only invalidates when actually-called functions change.
        fileOverrideHashKeys : List String
        fileOverrideHashKeys =
            fileOverrides |> List.map .hashKey

        semanticCacheKey : String
        semanticCacheKey =
            let
                -- Extract function references from the wrapper expression to find
                -- entry-point declarations, then use their semantic hashes (which
                -- transitively include all dependencies via the Merkle property)
                wrapperDeps =
                    case Elm.Parser.parseToFile wrapperSource of
                        Ok wrapperFile ->
                            wrapperFile.declarations
                                |> List.concatMap
                                    (\(Node _ decl) ->
                                        case decl of
                                            FunctionDeclaration func ->
                                                SemanticHash.extractDependencies
                                                    (Node
                                                        { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } }
                                                        (Node.value func.declaration |> .expression |> Node.value)
                                                    )

                                            _ ->
                                                []
                                    )

                        Err _ ->
                            []

                -- Resolve each dependency to its semantic hash from the index
                entryPointHashes =
                    wrapperDeps
                        |> List.filterMap
                            (\( modName, funcName ) ->
                                let
                                    qualName =
                                        if List.isEmpty modName then
                                            funcName

                                        else
                                            String.join "." modName ++ "." ++ funcName
                                in
                                SemanticHash.semanticHashForEntry project.semanticIndex qualName
                            )
                        |> List.sort

                overrideKey =
                    String.join "|" fileOverrideHashKeys

                sourceOverrideKey =
                    -- Hash each override over its full contents. The
                    -- previous `String.left 100` truncation collided
                    -- on overrides that shared a 100-char prefix.
                    sourceOverrides
                        |> List.map (\s -> String.fromInt (FNV1a.hash s))
                        |> String.join "|"
            in
            String.join "\n"
                (entryPointHashes ++ [ overrideKey, sourceOverrideKey, wrapperSource ])
    in
    Cache.do (Cache.writeFile semanticCacheKey Cache.succeed) <|
        \semanticHash ->
            Cache.compute [ "interpret-with-file-overrides", interpreterResultCacheVersion ]
                semanticHash
                (\() ->
                    let
                        -- Only parse sourceOverrides and wrapper (small/new);
                        -- user sources are pre-parsed in moduleGraph.moduleToFile
                        parsedOverrides : Result Types.Error (List File)
                        parsedOverrides =
                            (sourceOverrides ++ [ wrapperSource ])
                                |> List.map
                                    (\src ->
                                        Elm.Parser.parseToFile src
                                            |> Result.mapError Types.ParsingError
                                    )
                                |> combineFileResults
                    in
                    case parsedOverrides of
                        Err err ->
                            case err of
                                Types.ParsingError _ ->
                                    "ERROR: Parsing error"

                                Types.EvalError evalErr ->
                                    "ERROR: Eval error: "
                                        ++ evalErrorKindToString evalErr.error
                                        ++ " [module: "
                                        ++ String.join "." evalErr.currentModule
                                        ++ "] [stack: "
                                        ++ (evalErr.callStack
                                                |> List.take 10
                                                |> List.map (\ref -> String.join "." ref.moduleName ++ "." ++ ref.name)
                                                |> String.join " <- "
                                           )
                                        ++ "]"

                        Ok overrideFiles ->
                            let
                                overridesButWrapper =
                                    List.take (List.length overrideFiles - 1) overrideFiles

                                wrapperFile =
                                    List.drop (List.length overrideFiles - 1) overrideFiles

                                -- Incremental env: if baseUserEnv is available, replace only
                                -- the mutated module(s) instead of rebuilding from all user files.
                                result : Result Types.Error Types.Value
                                result =
                                    case project.baseUserEnv of
                                        Just baseEnv ->
                                            let
                                                -- Replace each file override in the base env
                                                replacedEnvResult =
                                                    fileOverrides
                                                        |> List.foldl
                                                            (\override envRes ->
                                                                envRes
                                                                    |> Result.andThen
                                                                        (\env ->
                                                                            Eval.Module.replaceModuleInEnv env
                                                                                { file = override.file
                                                                                , moduleName = Eval.Module.fileModuleName override.file
                                                                                , interface = Eval.Module.buildInterfaceFromFile override.file
                                                                                }
                                                                        )
                                                            )
                                                            (Ok baseEnv)

                                                -- Only need sourceOverrides + wrapper as additional files
                                                additionalFiles =
                                                    overridesButWrapper ++ wrapperFile
                                            in
                                            case replacedEnvResult of
                                                Ok updatedEnv ->
                                                    evalAdditionalFiles updatedEnv additionalFiles fileOverrides

                                                Err e ->
                                                    Err e

                                        Nothing ->
                                            -- Fallback: original path (all user files)
                                            let
                                                allFiles =
                                                    userFilteredFiles
                                                        ++ overridesButWrapper
                                                        ++ List.map .file fileOverrides
                                                        ++ wrapperFile
                                            in
                                            evalAdditionalFiles project.packageEnv allFiles fileOverrides
                            in
                            case result of
                                Ok (Types.String s) ->
                                    s

                                Ok _ ->
                                    "ERROR: Expected String result"

                                Err (Types.ParsingError _) ->
                                    "ERROR: Parsing error"

                                Err (Types.EvalError evalErr) ->
                                    "ERROR: Eval error: "
                                        ++ evalErrorKindToString evalErr.error
                                        ++ " [module: "
                                        ++ String.join "." evalErr.currentModule
                                        ++ "] [stack: "
                                        ++ (evalErr.callStack
                                                |> List.take 10
                                                |> List.map (\ref -> String.join "." ref.moduleName ++ "." ++ ref.name)
                                                |> String.join " <- "
                                           )
                                        ++ "]"
                )
                k


{-| Cache wrapper around an externally-computed eval result. Same
semantic-hash key as `evalWithFileOverrides` (no `fileOverrides` —
worker dispatch can't carry mutation-testing overrides), so cache files
are interchangeable: a warm run created by `evalWithFileOverrides`
hits the cache here too, and vice versa.

The `compute` argument is the async work that produces the eval output
*on cache miss only*. The test runner passes a `BackendTask.Parallel.runOn`
dispatch here — main thread checks the cache, hits short-circuit and
the worker pool stays idle; misses dispatch to a worker that runs the
eval and the result gets written back to cache.

Returns the path to the cached output file (the eval result string),
ready to be read with `BackendTask.File.rawFile`.

-}
evalCachedViaTask :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , compute : BackendTask FatalError String
        }
    -> Cache.Monad FileOrDirectory
evalCachedViaTask (InterpreterProject project) { imports, expression, sourceOverrides, compute } =
    let
        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        semanticCacheKey : String
        semanticCacheKey =
            buildEvalCacheKey project sourceOverrides wrapperSource
    in
    Cache.do (Cache.writeFile semanticCacheKey Cache.succeed) <|
        \semanticHash ->
            Cache.computeAsync
                [ "interpret-with-file-overrides", interpreterResultCacheVersion ]
                semanticHash
                compute
                Cache.succeed


{-| Compute the same semantic cache key `evalWithFileOverrides` uses,
factored out so `evalCachedViaTask` can reproduce it exactly (so the
two paths share cache files). Always assumes empty `fileOverrides` —
worker dispatch can't carry per-file source replacements.
-}
buildEvalCacheKey :
    { a
        | semanticIndex : SemanticHash.DeclarationIndex
    }
    -> List String
    -> String
    -> String
buildEvalCacheKey project sourceOverrides wrapperSource =
    let
        wrapperDeps : List ( ModuleName, String )
        wrapperDeps =
            case Elm.Parser.parseToFile wrapperSource of
                Ok wrapperFile ->
                    wrapperFile.declarations
                        |> List.concatMap
                            (\(Node _ decl) ->
                                case decl of
                                    FunctionDeclaration func ->
                                        SemanticHash.extractDependencies
                                            (Node
                                                { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } }
                                                (Node.value func.declaration |> .expression |> Node.value)
                                            )

                                    _ ->
                                        []
                            )

                Err _ ->
                    []

        entryPointHashes : List String
        entryPointHashes =
            wrapperDeps
                |> List.filterMap
                    (\( modName, funcName ) ->
                        let
                            qualName : String
                            qualName =
                                if List.isEmpty modName then
                                    funcName

                                else
                                    String.join "." modName ++ "." ++ funcName
                        in
                        SemanticHash.semanticHashForEntry project.semanticIndex qualName
                    )
                |> List.sort

        overrideKey : String
        overrideKey =
            -- Always empty for evalCachedViaTask — fileOverrides is unsupported.
            ""

        sourceOverrideKey : String
        sourceOverrideKey =
            sourceOverrides
                |> List.map (\s -> String.fromInt (FNV1a.hash s))
                |> String.join "|"
    in
    String.join "\n"
        (entryPointHashes ++ [ overrideKey, sourceOverrideKey, wrapperSource ])


{-| Run the test runner's `results` expression against an extended
project env. Routes through the resolved-IR evaluator
(`evalWithResolvedIRFromFilesAndIntercepts`) when there are no file
overrides — the test-runner cold path — and falls back to the old
string-keyed evaluator when there are. The mutation-testing path uses
file overrides + `replaceModuleInEnv` which doesn't currently refresh
the precomputed `ResolvedProject`, so feeding it through the resolved-
IR evaluator would see stale function bodies. The fallback is safe
and the dev-loop scenarios it covers are already at parity with
elm-test.
-}
evalAdditionalFiles :
    Eval.Module.ProjectEnv
    -> List File
    -> List { file : File, hashKey : String }
    -> Result Types.Error Types.Value
evalAdditionalFiles env additionalFiles fileOverrides =
    if List.isEmpty fileOverrides then
        case
            Eval.Module.evalWithResolvedIRFromFilesAndIntercepts
                env
                additionalFiles
                FastDict.empty
                FastDict.empty
                (FunctionOrValue [] "results")
        of
            Types.EvOk value ->
                Ok value

            Types.EvErr errorData ->
                Err (Types.EvalError errorData)

            _ ->
                Err
                    (Types.EvalError
                        { currentModule = []
                        , callStack = []
                        , error = Types.Unsupported "evalWithResolvedIRFromFilesAndIntercepts returned a trace/yield/memo result; not supported on the test-runner path"
                        }
                    )

    else
        Eval.Module.evalWithEnvFromFilesAndLimit
            (Just 5000000)
            env
            additionalFiles
            (FunctionOrValue [] "results")


{-| Evaluate a test expression with tracing to collect coverage data.

Runs the test suite once (unmutated) with the interpreter's trace mode enabled,
then walks the resulting CallTree to extract all evaluated source ranges.
Returns both the test result string and the list of covered ranges.

-}
evalSimple :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        }
    -> BackendTask FatalError String
evalSimple (InterpreterProject project) { imports, expression, sourceOverrides } =
    let
        wrapperSource =
            generateWrapper imports expression

        result =
            case project.baseUserEnv of
                Just baseEnv ->
                    -- Fast path: baseUserEnv already contains every user
                    -- module loaded from loadWithProfile. Parse the
                    -- sourceOverrides + wrapper into Files and route
                    -- through `evalAdditionalFiles`, which dispatches to
                    -- the resolved-IR evaluator
                    -- (`evalWithResolvedIRFromFilesAndIntercepts`) when
                    -- there are no fileOverrides — the same hot path
                    -- `evalWithFileOverrides` uses for the test runner's
                    -- cold cache miss. Going through the string-keyed
                    -- `evalWithEnv` instead was a 3-4x slowdown on
                    -- ListTests-sized suites (see
                    -- .scratch/parallel-ceiling.md, 2026-04-20 profile).
                    case parseSourcesAsFiles (sourceOverrides ++ [ wrapperSource ]) of
                        Ok additionalFiles ->
                            evalAdditionalFiles baseEnv additionalFiles []

                        Err err ->
                            Err err

                Nothing ->
                    let
                        wrapperImports =
                            DepGraph.parseImports wrapperSource |> Set.fromList

                        neededModules =
                            transitiveModuleDeps project.moduleGraph.imports wrapperImports

                        filteredSources =
                            topoSortModules project.moduleGraph neededModules

                        userFilteredSources =
                            filteredSources
                                |> List.filter
                                    (\src ->
                                        case DepGraph.parseModuleName src of
                                            Just name ->
                                                not (Set.member name project.packageModuleNames)

                                            Nothing ->
                                                True
                                    )

                        allSources =
                            userFilteredSources ++ sourceOverrides ++ [ wrapperSource ]
                    in
                    Eval.Module.evalWithEnv
                        project.packageEnv
                        allSources
                        (FunctionOrValue [] "results")
    in
    BackendTask.succeed
        (case result of
            Ok (Types.String s) ->
                s

            Ok _ ->
                "ERROR: Expected String result"

            Err (Types.ParsingError _) ->
                "ERROR: Parsing error"

            Err (Types.EvalError evalErr) ->
                "ERROR: Eval error: "
                    ++ evalErrorKindToString evalErr.error
                    ++ " [module: "
                    ++ String.join "." evalErr.currentModule
                    ++ "] [stack: "
                    ++ (evalErr.callStack
                            |> List.take 10
                            |> List.map (\ref -> String.join "." ref.moduleName ++ "." ++ ref.name)
                            |> String.join " <- "
                       )
                    ++ "]"
        )


evalWithCoverage :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , probeLines : Set Int
        }
    -> BackendTask FatalError { result : String, coveredRanges : List Range }
evalWithCoverage (InterpreterProject project) { imports, expression, sourceOverrides, probeLines } =
    let
        wrapperSource : String
        wrapperSource =
            generateWrapper imports expression

        ( result, coveredRanges ) =
            case project.baseUserEnv of
                Just baseEnv ->
                    -- Fast path: see evalSimple for rationale.
                    Eval.Module.coverageWithEnvAndLimit Nothing
                        probeLines
                        baseEnv
                        (sourceOverrides ++ [ wrapperSource ])
                        (FunctionOrValue [] "results")

                Nothing ->
                    let
                        wrapperImports : Set String
                        wrapperImports =
                            DepGraph.parseImports wrapperSource |> Set.fromList

                        neededModules : Set String
                        neededModules =
                            transitiveModuleDeps project.moduleGraph.imports wrapperImports

                        filteredSources : List String
                        filteredSources =
                            topoSortModules project.moduleGraph neededModules

                        userFilteredSources : List String
                        userFilteredSources =
                            filteredSources
                                |> List.filter
                                    (\src ->
                                        case DepGraph.parseModuleName src of
                                            Just name ->
                                                not (Set.member name project.packageModuleNames)

                                            Nothing ->
                                                True
                                    )

                        allSources =
                            userFilteredSources ++ sourceOverrides ++ [ wrapperSource ]
                    in
                    Eval.Module.coverageWithEnvAndLimit Nothing
                        probeLines
                        project.packageEnv
                        allSources
                        (FunctionOrValue [] "results")

        resultString =
            case result of
                Ok (Types.String s) ->
                    s

                Ok _ ->
                    "ERROR: Expected String result"

                Err (Types.ParsingError _) ->
                    "ERROR: Parsing error"

                Err (Types.EvalError evalErr) ->
                    "ERROR: Eval error: "
                        ++ evalErrorKindToString evalErr.error
                        ++ " [module: "
                        ++ String.join "." evalErr.currentModule
                        ++ "] [stack: "
                        ++ (evalErr.callStack
                                |> List.take 10
                                |> List.map (\ref -> String.join "." ref.moduleName ++ "." ++ ref.name)
                                |> String.join " <- "
                           )
                        ++ "]"
    in
    BackendTask.succeed { result = resultString, coveredRanges = coveredRanges }


combineFileResults : List (Result e a) -> Result e (List a)
combineFileResults results =
    List.foldr
        (\result acc ->
            Result.map2 (::) result acc
        )
        (Ok [])
        results


{-| Parse a list of source strings into `Elm.Syntax.File.File` ASTs,
short-circuiting to a single `Types.ParsingError` on the first parse
failure. Used by `evalSimple`'s fast path to feed the resolved-IR
evaluator (which takes pre-parsed Files, not source strings).
-}
parseSourcesAsFiles : List String -> Result Types.Error (List File)
parseSourcesAsFiles sources =
    sources
        |> List.map
            (\src ->
                Elm.Parser.parseToFile src
                    |> Result.mapError Types.ParsingError
            )
        |> combineFileResults


evalErrorKindToString : Types.EvalErrorKind -> String
evalErrorKindToString kind =
    case kind of
        Types.TypeError msg ->
            "type error: " ++ msg

        Types.Unsupported msg ->
            "unsupported: " ++ msg

        Types.NameError name ->
            "could not resolve '" ++ name ++ "'"

        Types.Todo msg ->
            "hit Debug.todo: " ++ msg

        Types.TailCall _ ->
            "internal TCO signal (should not be user-visible)"


{-| Get the pre-built package environment. Useful for direct eval calls
that bypass the caching layer (e.g. benchmarking).
-}
getPackageEnv : InterpreterProject -> Eval.Module.ProjectEnv
getPackageEnv (InterpreterProject project) =
    project.packageEnv


precomputedValuesCount : InterpreterProject -> Int
precomputedValuesCount (InterpreterProject project) =
    case project.baseUserEnv of
        Just env ->
            Eval.Module.precomputedValuesCount env

        Nothing ->
            Eval.Module.precomputedValuesCount project.packageEnv


precomputedValuesByModule : InterpreterProject -> List ( String, Int )
precomputedValuesByModule (InterpreterProject project) =
    case project.baseUserEnv of
        Just env ->
            Eval.Module.precomputedValuesByModule env

        Nothing ->
            Eval.Module.precomputedValuesByModule project.packageEnv


{-| Get the dependency graph for the project's user source files.
-}
getDepGraph : InterpreterProject -> DepGraph.Graph
getDepGraph (InterpreterProject project) =
    project.depGraph


{-| Get the module graph (parsed user files + import edges) for the project.
Used to ship pre-built graph state to worker pools so they can skip
the `build_graph` phase of `loadWith`.
-}
getModuleGraph : InterpreterProject -> ModuleGraph
getModuleGraph (InterpreterProject project) =
    project.moduleGraph


{-| Prepare the source lists needed for eval, without actually evaluating.

Returns `allSources` (everything needed, for `evalProject`) and
`userSources` (only non-package modules, for `evalWithEnv`).

-}
prepareEvalSources :
    InterpreterProject
    -> { imports : List String, expression : String }
    -> { allSources : List String, userSources : List String }
prepareEvalSources (InterpreterProject project) { imports, expression } =
    let
        wrapperSource =
            generateWrapper imports expression

        wrapperImports =
            DepGraph.parseImports wrapperSource |> Set.fromList

        neededModules =
            transitiveModuleDeps project.moduleGraph.imports wrapperImports

        filteredSources =
            topoSortModules project.moduleGraph neededModules

        userFilteredSources =
            filteredSources
                |> List.filter
                    (\src ->
                        case DepGraph.parseModuleName src of
                            Just name ->
                                not (Set.member name project.packageModuleNames)

                            Nothing ->
                                True
                    )
    in
    { allSources = filteredSources ++ [ wrapperSource ]
    , userSources = userFilteredSources ++ [ wrapperSource ]
    }


{-| Prepare and evaluate an expression, pure function without Cache.Monad.

Uses baseUserEnv when available (skips re-parsing all user modules).
Only parses sourceOverrides + the generated wrapper module.

Useful when the caller manages caching externally (e.g. using semantic hash
keys instead of content-based hashing).

-}
prepareAndEval :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String }
    -> Result String String
prepareAndEval (InterpreterProject project) { imports, expression, sourceOverrides } =
    let
        wrapperSource =
            generateWrapper imports expression

        -- Parse only sourceOverrides + wrapper (small, new each time).
        -- User modules are already in baseUserEnv.
        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults

        result =
            case parsedNewModules of
                Err err ->
                    Err err

                Ok newFiles ->
                    case project.baseUserEnv of
                        Just baseEnv ->
                            -- Fast path: baseUserEnv already has all user modules.
                            -- Only need to add sourceOverrides + wrapper.
                            Eval.Module.evalWithEnvFromFiles baseEnv newFiles (FunctionOrValue [] "results")

                        Nothing ->
                            -- Fallback: parse everything from scratch
                            let
                                { userSources } =
                                    prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                                allSources =
                                    let
                                        len =
                                            List.length userSources

                                        beforeWrapper =
                                            List.take (len - 1) userSources

                                        wrapper =
                                            List.drop (len - 1) userSources
                                    in
                                    beforeWrapper ++ sourceOverrides ++ wrapper
                            in
                            Eval.Module.evalWithEnv project.packageEnv allSources (FunctionOrValue [] "results")
    in
    formatEvalResult result


{-| Like prepareAndEval but returns the raw interpreter Value.

Useful when the expression returns structured data (e.g. a Tuple)
that the caller needs to decompose.

-}
prepareAndEvalRaw :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String }
    -> Result String Types.Value
prepareAndEvalRaw (InterpreterProject project) { imports, expression, sourceOverrides } =
    let
        wrapperSource =
            generateWrapper imports expression

        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults
    in
    case parsedNewModules of
        Err err ->
            Err (formatError err)

        Ok newFiles ->
            case project.baseUserEnv of
                Just baseEnv ->
                    Eval.Module.evalWithEnvFromFiles baseEnv newFiles (FunctionOrValue [] "results")
                        |> Result.mapError formatError

                Nothing ->
                    let
                        { userSources } =
                            prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                        allSources =
                            let
                                len =
                                    List.length userSources

                                beforeWrapper =
                                    List.take (len - 1) userSources

                                wrapper =
                                    List.drop (len - 1) userSources
                            in
                            beforeWrapper ++ sourceOverrides ++ wrapper
                    in
                    Eval.Module.evalWithEnv project.packageEnv allSources (FunctionOrValue [] "results")
                        |> Result.mapError formatError


{-| Like prepareAndEval but with injected Values available in the expression.

The injected values are available as local variables. Used to pass
preserved interpreter state (like elm-review's updatedRules) into
subsequent evaluations.

-}
prepareAndEvalWithValues :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, injectedValues : FastDict.Dict String Types.Value }
    -> Result String Types.Value
prepareAndEvalWithValues (InterpreterProject project) { imports, expression, sourceOverrides, injectedValues } =
    let
        wrapperSource =
            generateWrapper imports expression

        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults
    in
    case parsedNewModules of
        Err err ->
            Err (formatError err)

        Ok newFiles ->
            case project.baseUserEnv of
                Just baseEnv ->
                    Eval.Module.evalWithEnvFromFilesAndValues baseEnv newFiles injectedValues (FunctionOrValue [] "results")
                        |> Result.mapError formatError

                Nothing ->
                    -- Fallback without values (can't inject without baseUserEnv)
                    Eval.Module.evalWithEnvFromFiles project.packageEnv newFiles (FunctionOrValue [] "results")
                        |> Result.mapError formatError


{-| Like `prepareAndEvalRaw`, but uses the interpreter's internal memo runtime
for selected qualified function names and returns the updated memo cache so it
can be reused across later invocations.
-}
prepareAndEvalWithMemoizedFunctions :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , memoizedFunctions : Set String
        , memoCache : MemoRuntime.MemoCache
        , collectMemoStats : Bool
        }
    ->
        Result
            String
            { value : Types.Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
prepareAndEvalWithMemoizedFunctions (InterpreterProject project) { imports, expression, sourceOverrides, memoizedFunctions, memoCache, collectMemoStats } =
    let
        wrapperSource =
            generateWrapper imports expression

        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults
    in
    case parsedNewModules of
        Err err ->
            Err (formatError err)

        Ok newFiles ->
            case project.baseUserEnv of
                Just baseEnv ->
                    Eval.Module.evalWithEnvFromFilesAndMemo baseEnv newFiles memoizedFunctions memoCache collectMemoStats (FunctionOrValue [] "results")
                        |> Result.mapError formatError

                Nothing ->
                    let
                        { userSources } =
                            prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                        allSources =
                            let
                                len =
                                    List.length userSources

                                beforeWrapper =
                                    List.take (len - 1) userSources

                                wrapper =
                                    List.drop (len - 1) userSources
                            in
                            beforeWrapper ++ sourceOverrides ++ wrapper
                    in
                    Eval.Module.evalWithMemoizedFunctions project.packageEnv allSources memoizedFunctions memoCache collectMemoStats (FunctionOrValue [] "results")
                        |> Result.mapError formatError


prepareAndEvalWithValuesAndMemoizedFunctions :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , injectedValues : FastDict.Dict String Types.Value
        , memoizedFunctions : Set String
        , memoCache : MemoRuntime.MemoCache
        , collectMemoStats : Bool
        }
    ->
        Result
            String
            { value : Types.Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
prepareAndEvalWithValuesAndMemoizedFunctions (InterpreterProject project) { imports, expression, sourceOverrides, injectedValues, memoizedFunctions, memoCache, collectMemoStats } =
    let
        wrapperSource =
            generateWrapper imports expression

        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults
    in
    case parsedNewModules of
        Err err ->
            Err (formatError err)

        Ok newFiles ->
            case project.baseUserEnv of
                Just baseEnv ->
                    Eval.Module.evalWithEnvFromFilesAndValuesAndMemo baseEnv newFiles injectedValues memoizedFunctions memoCache collectMemoStats (FunctionOrValue [] "results")
                        |> Result.mapError formatError

                Nothing ->
                    let
                        { userSources } =
                            prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                        allSources =
                            let
                                len =
                                    List.length userSources

                                beforeWrapper =
                                    List.take (len - 1) userSources

                                wrapper =
                                    List.drop (len - 1) userSources
                            in
                            beforeWrapper ++ sourceOverrides ++ wrapper
                    in
                    Eval.Module.evalWithValuesAndMemoizedFunctions project.packageEnv allSources injectedValues memoizedFunctions memoCache collectMemoStats (FunctionOrValue [] "results")
                        |> Result.mapError formatError


{-| Evaluate with intercepts that can yield to the framework.

When an intercept yields (EvYield tag payload resume), the yieldHandler
BackendTask runs with (tag, payload), producing a Value. The eval then
resumes with that Value via the continuation.

This is the BackendTask driver loop: eval → yield → handle → resume → repeat.

-}
prepareRawEvalWithYield :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, intercepts : FastDict.Dict String Types.Intercept, injectedValues : FastDict.Dict String Types.Value }
    -> Types.EvalResult Types.Value
prepareRawEvalWithYield (InterpreterProject project) { imports, expression, sourceOverrides, intercepts, injectedValues } =
    prepareRawEvalWithYieldAndMemo
        (InterpreterProject project)
        { imports = imports
        , expression = expression
        , sourceOverrides = sourceOverrides
        , intercepts = intercepts
        , injectedValues = injectedValues
        , memoizedFunctions = Set.empty
        }


prepareRawEvalWithYieldAndMemo :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , intercepts : FastDict.Dict String Types.Intercept
        , injectedValues : FastDict.Dict String Types.Value
        , memoizedFunctions : Set String
        }
    -> Types.EvalResult Types.Value
prepareRawEvalWithYieldAndMemo (InterpreterProject project) { imports, expression, sourceOverrides, intercepts, injectedValues, memoizedFunctions } =
    let
        wrapperSource =
            generateWrapper imports expression

        allSources =
            let
                { userSources } =
                    prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                len =
                    List.length userSources

                beforeWrapper =
                    List.take (len - 1) userSources

                wrapper =
                    List.drop (len - 1) userSources
            in
            beforeWrapper ++ sourceOverrides ++ wrapper

        evalSources =
            case project.baseUserEnv of
                Just _ ->
                    sourceOverrides ++ [ wrapperSource ]

                Nothing ->
                    allSources

        env =
            case project.baseUserEnv of
                Just baseEnv ->
                    baseEnv

                Nothing ->
                    project.packageEnv

        useResolvedIR : Bool
        useResolvedIR =
            case project.envMode of
                LegacyAst ->
                    False

                ResolvedListUnplanned ->
                    True

                ResolvedListSlotted ->
                    True

        rawResult =
            if useResolvedIR && Set.isEmpty memoizedFunctions then
                -- Phase 1c: route yield-driving evals through the resolved-IR
                -- entry point when the project's envMode selects it. Memo is
                -- still gated out because `evalWithResolvedIRFromFilesAndIntercepts`
                -- doesn't support memoizedFunctions; the else-branches handle
                -- memo via the legacy path.
                let
                    parseResult =
                        evalSources
                            |> List.map
                                (\src ->
                                    Elm.Parser.parseToFile src
                                        |> Result.mapError Types.ParsingError
                                )
                            |> combineFileResults
                in
                case parseResult of
                    Err _ ->
                        Types.EvErr { currentModule = [], callStack = [], error = Types.TypeError "Parse error in prepareAndEvalWithYield (resolved IR path)" }

                    Ok parsedModules ->
                        Eval.Module.evalWithResolvedIRFromFilesAndIntercepts
                            env
                            parsedModules
                            injectedValues
                            intercepts
                            (FunctionOrValue [] "results")

            else if FastDict.isEmpty injectedValues then
                Eval.Module.evalWithInterceptsAndMemoRaw env evalSources intercepts memoizedFunctions (FunctionOrValue [] "results")

            else
                -- Need to inject values AND use intercepts.
                -- Parse sources, build env, inject values, then eval with intercepts.
                -- This is a combination of evalWithEnvFromFilesAndValues + evalWithInterceptsRaw.
                let
                    parseResult =
                        evalSources
                            |> List.map
                                (\src ->
                                    Elm.Parser.parseToFile src
                                        |> Result.mapError Types.ParsingError
                                )
                            |> combineFileResults
                in
                case parseResult of
                    Err _ ->
                        Types.EvErr { currentModule = [], callStack = [], error = Types.TypeError "Parse error in prepareAndEvalWithYield" }

                    Ok parsedModules ->
                        Eval.Module.evalWithEnvFromFilesAndValuesAndInterceptsAndMemoRaw env parsedModules injectedValues intercepts memoizedFunctions (FunctionOrValue [] "results")
    in
    rawResult


{-| Convert the `EvalResult Value` returned by the new
`evalWithResolvedIRFromFilesAndIntercepts` entry point into the
`Result String Value` shape callers of `prepareAndEvalWithIntercepts`
expect. Only the synchronous `EvOk` / `EvErr` variants can be
translated directly; an intercept that returns `EvYield` / `EvMemoLookup`
/ `EvMemoStore` through this synchronous path would be a programming
error (no yield handler is installed), so we surface it as a formatted
error string to make it obvious if it happens.
-}
resolvedEvalResultToResult : Types.EvalResult Types.Value -> Result String Types.Value
resolvedEvalResultToResult result =
    case result of
        Types.EvOk value ->
            Ok value

        Types.EvErr errorData ->
            Err
                (formatError
                    (Types.EvalError errorData)
                )

        Types.EvOkTrace value _ _ ->
            Ok value

        Types.EvErrTrace errorData _ _ ->
            Err (formatError (Types.EvalError errorData))

        Types.EvYield tag _ _ ->
            Err ("ERROR: Unexpected EvYield '" ++ tag ++ "' in synchronous intercept path (resolvedEvalResultToResult)")

        Types.EvMemoLookup _ _ ->
            Err "ERROR: Unexpected EvMemoLookup in synchronous intercept path (resolvedEvalResultToResult)"

        Types.EvMemoStore _ _ ->
            Err "ERROR: Unexpected EvMemoStore in synchronous intercept path (resolvedEvalResultToResult)"

        Types.EvOkCoverage value _ ->
            Ok value

        Types.EvErrCoverage errorData _ ->
            Err (formatError (Types.EvalError errorData))


{-| Evaluate with intercepts that can yield to the framework.

When an intercept yields (EvYield tag payload resume), the yieldHandler
BackendTask runs with (tag, payload), producing a Value. The eval then
resumes with that Value via the continuation.

This is the BackendTask driver loop: eval → yield → handle → resume → repeat.

-}
prepareAndEvalWithYield :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, intercepts : FastDict.Dict String Types.Intercept, injectedValues : FastDict.Dict String Types.Value }
    -> (String -> Types.Value -> BackendTask FatalError Types.Value)
    -> BackendTask FatalError (Result String Types.Value)
prepareAndEvalWithYield project evalConfig yieldHandler =
    driveYields yieldHandler (prepareRawEvalWithYield project evalConfig)


prepareAndEvalWithYieldAndMemoizedFunctions :
    InterpreterProject
    ->
        { imports : List String
        , expression : String
        , sourceOverrides : List String
        , intercepts : FastDict.Dict String Types.Intercept
        , injectedValues : FastDict.Dict String Types.Value
        , memoizedFunctions : Set String
        , memoCache : MemoRuntime.MemoCache
        , collectMemoStats : Bool
        }
    -> (String -> Types.Value -> BackendTask FatalError Types.Value)
    ->
        BackendTask
            FatalError
            { result : Result String Types.Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
prepareAndEvalWithYieldAndMemoizedFunctions project evalConfig yieldHandler =
    driveYieldsAndMemo
        evalConfig.memoCache
        (if evalConfig.collectMemoStats then
            MemoRuntime.emptyMemoStats

         else
            MemoRuntime.disabledMemoStats
        )
        yieldHandler
        (prepareRawEvalWithYieldAndMemo project
            { imports = evalConfig.imports
            , expression = evalConfig.expression
            , sourceOverrides = evalConfig.sourceOverrides
            , intercepts = evalConfig.intercepts
            , injectedValues = evalConfig.injectedValues
            , memoizedFunctions = evalConfig.memoizedFunctions
            }
        )


{-| Like `prepareAndEvalWithYield`, but threads caller-managed state through the
yield loop. Useful for in-memory caching experiments where the state should
live outside the interpreter and be updated on each yield.
-}
prepareAndEvalWithYieldState :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, intercepts : FastDict.Dict String Types.Intercept, injectedValues : FastDict.Dict String Types.Value }
    -> state
    -> (state -> String -> Types.Value -> BackendTask FatalError ( state, Types.Value ))
    -> BackendTask FatalError ( Result String Types.Value, state )
prepareAndEvalWithYieldState project evalConfig initialState yieldHandler =
    driveYieldsState initialState yieldHandler (prepareRawEvalWithYield project evalConfig)


{-| Drive the yield loop: handle EvYield via BackendTask, resume, repeat.
-}
driveYields :
    (String -> Types.Value -> BackendTask FatalError Types.Value)
    -> Types.EvalResult Types.Value
    -> BackendTask FatalError (Result String Types.Value)
driveYields yieldHandler evalResult =
    case evalResult of
        Types.EvOk value ->
            BackendTask.succeed (Ok value)

        Types.EvErr evalErr ->
            BackendTask.succeed (Err (formatError (Types.EvalError evalErr)))

        Types.EvOkTrace value _ _ ->
            BackendTask.succeed (Ok value)

        Types.EvErrTrace evalErr _ _ ->
            BackendTask.succeed (Err (formatError (Types.EvalError evalErr)))

        Types.EvMemoLookup _ resume ->
            driveYields yieldHandler (resume Nothing)

        Types.EvMemoStore _ next ->
            driveYields yieldHandler next

        Types.EvYield tag payload resume ->
            -- Handle the yield via BackendTask, then resume eval
            yieldHandler tag payload
                |> BackendTask.andThen
                    (\resumeValue ->
                        driveYields yieldHandler (resume resumeValue)
                    )

        Types.EvOkCoverage value _ ->
            BackendTask.succeed (Ok value)

        Types.EvErrCoverage evalErr _ ->
            BackendTask.succeed (Err (formatError (Types.EvalError evalErr)))


driveYieldsState :
    state
    -> (state -> String -> Types.Value -> BackendTask FatalError ( state, Types.Value ))
    -> Types.EvalResult Types.Value
    -> BackendTask FatalError ( Result String Types.Value, state )
driveYieldsState state yieldHandler evalResult =
    case evalResult of
        Types.EvOk value ->
            BackendTask.succeed ( Ok value, state )

        Types.EvErr evalErr ->
            BackendTask.succeed ( Err (formatError (Types.EvalError evalErr)), state )

        Types.EvOkTrace value _ _ ->
            BackendTask.succeed ( Ok value, state )

        Types.EvErrTrace evalErr _ _ ->
            BackendTask.succeed ( Err (formatError (Types.EvalError evalErr)), state )

        Types.EvMemoLookup _ resume ->
            driveYieldsState state yieldHandler (resume Nothing)

        Types.EvMemoStore _ next ->
            driveYieldsState state yieldHandler next

        Types.EvYield tag payload resume ->
            yieldHandler state tag payload
                |> BackendTask.andThen
                    (\( nextState, resumeValue ) ->
                        driveYieldsState nextState yieldHandler (resume resumeValue)
                    )

        Types.EvOkCoverage value _ ->
            BackendTask.succeed ( Ok value, state )

        Types.EvErrCoverage evalErr _ ->
            BackendTask.succeed ( Err (formatError (Types.EvalError evalErr)), state )


driveYieldsAndMemo :
    MemoRuntime.MemoCache
    -> MemoRuntime.MemoStats
    -> (String -> Types.Value -> BackendTask FatalError Types.Value)
    -> Types.EvalResult Types.Value
    ->
        BackendTask
            FatalError
            { result : Result String Types.Value
            , memoCache : MemoRuntime.MemoCache
            , memoStats : MemoRuntime.MemoStats
            }
driveYieldsAndMemo memoCache memoStats yieldHandler evalResult =
    case evalResult of
        Types.EvOk value ->
            BackendTask.succeed
                { result = Ok value
                , memoCache = memoCache
                , memoStats = memoStats
                }

        Types.EvErr evalErr ->
            BackendTask.succeed
                { result = Err (formatError (Types.EvalError evalErr))
                , memoCache = memoCache
                , memoStats = memoStats
                }

        Types.EvOkTrace value _ _ ->
            BackendTask.succeed
                { result = Ok value
                , memoCache = memoCache
                , memoStats = memoStats
                }

        Types.EvErrTrace evalErr _ _ ->
            BackendTask.succeed
                { result = Err (formatError (Types.EvalError evalErr))
                , memoCache = memoCache
                , memoStats = memoStats
                }

        Types.EvMemoLookup payload resume ->
            let
                ( nextCache, nextStats, maybeValue ) =
                    Eval.Module.handleInternalMemoLookup memoCache memoStats payload
            in
            driveYieldsAndMemo nextCache nextStats yieldHandler (resume maybeValue)

        Types.EvMemoStore payload next ->
            let
                ( nextCache, nextStats ) =
                    Eval.Module.handleInternalMemoStore memoCache memoStats payload
            in
            driveYieldsAndMemo nextCache nextStats yieldHandler next

        Types.EvYield tag payload resume ->
            case Eval.Module.handleInternalMemoYield memoCache memoStats tag payload of
                Just ( nextCache, nextStats, resumeValue ) ->
                    driveYieldsAndMemo nextCache nextStats yieldHandler (resume resumeValue)

                Nothing ->
                    yieldHandler tag payload
                        |> BackendTask.andThen
                            (\resumeValue ->
                                driveYieldsAndMemo memoCache memoStats yieldHandler (resume resumeValue)
                            )

        Types.EvOkCoverage value _ ->
            BackendTask.succeed
                { result = Ok value
                , memoCache = memoCache
                , memoStats = memoStats
                }

        Types.EvErrCoverage evalErr _ ->
            BackendTask.succeed
                { result = Err (formatError (Types.EvalError evalErr))
                , memoCache = memoCache
                , memoStats = memoStats
                }


{-| Like prepareAndEval but with function intercepts (synchronous, no yield support).

Intercepts are checked before normal function evaluation. Used for
elm-review cache markers, memoization, and framework callbacks.

-}
prepareAndEvalWithIntercepts :
    InterpreterProject
    -> { imports : List String, expression : String, sourceOverrides : List String, intercepts : FastDict.Dict String Types.Intercept }
    -> Result String Types.Value
prepareAndEvalWithIntercepts (InterpreterProject project) { imports, expression, sourceOverrides, intercepts } =
    let
        wrapperSource =
            generateWrapper imports expression

        parsedNewModules =
            (sourceOverrides ++ [ wrapperSource ])
                |> List.map
                    (\src ->
                        Elm.Parser.parseToFile src
                            |> Result.mapError Types.ParsingError
                    )
                |> combineFileResults
    in
    case parsedNewModules of
        Err err ->
            Err (formatError err)

        Ok newFiles ->
            let
                -- Build sources list for evalWithIntercepts
                { userSources } =
                    prepareEvalSources (InterpreterProject project) { imports = imports, expression = expression }

                allSources =
                    let
                        len =
                            List.length userSources

                        beforeWrapper =
                            List.take (len - 1) userSources

                        wrapper =
                            List.drop (len - 1) userSources
                    in
                    beforeWrapper ++ sourceOverrides ++ wrapper
            in
            case project.baseUserEnv of
                Just baseEnv ->
                    case project.envMode of
                        LegacyAst ->
                            -- Fast path: baseUserEnv has all user modules pre-loaded.
                            -- Only parse sourceOverrides + wrapper (small/new).
                            Eval.Module.evalWithIntercepts baseEnv (sourceOverrides ++ [ wrapperSource ]) intercepts (FunctionOrValue [] "results")
                                |> Result.mapError formatError

                        _ ->
                            Eval.Module.evalWithResolvedIRFromFilesAndIntercepts
                                baseEnv
                                newFiles
                                FastDict.empty
                                intercepts
                                (FunctionOrValue [] "results")
                                |> resolvedEvalResultToResult

                Nothing ->
                    -- Fallback: parse everything from scratch. We don't
                    -- route this through the new path because the new
                    -- entry point assumes the base env already has the
                    -- user modules built in — without a baseUserEnv,
                    -- reusing it would silently miss user declarations.
                    Eval.Module.evalWithIntercepts project.packageEnv allSources intercepts (FunctionOrValue [] "results")
                        |> Result.mapError formatError


formatEvalResult : Result Types.Error Types.Value -> Result String String
formatEvalResult result =
    case result of
        Ok (Types.String s) ->
            Ok s

        Ok _ ->
            Err "ERROR: Expected String result"

        Err err ->
            Err (formatError err)


formatError : Types.Error -> String
formatError err =
    case err of
        Types.ParsingError _ ->
            "ERROR: Parsing error"

        Types.EvalError evalErr ->
            "ERROR: Eval error: "
                ++ evalErrorKindToString evalErr.error
                ++ " [module: "
                ++ String.join "." evalErr.currentModule
                ++ "] [stack: "
                ++ (evalErr.callStack |> List.take 10 |> List.map (\ref -> String.join "." ref.moduleName ++ "." ++ ref.name) |> String.join " <- ")
                ++ "]"



-- HELPERS


{-| Parse "ModuleName.valueName" into its parts.
-}
parseExpression : String -> Maybe ( String, String )
parseExpression expr =
    let
        trimmed =
            String.trim expr
    in
    case String.split "." trimmed |> List.reverse of
        valueName :: modulePartsReversed ->
            if List.isEmpty modulePartsReversed then
                Nothing

            else
                let
                    moduleName =
                        modulePartsReversed |> List.reverse |> String.join "."
                in
                if String.isEmpty valueName || String.isEmpty moduleName then
                    Nothing

                else
                    Just ( moduleName, valueName )

        _ ->
            Nothing


{-| Generate a wrapper module that imports the given modules and evaluates
the expression as `results : String`.
-}
generateWrapper : List String -> String -> String
generateWrapper imports expression =
    let
        importLines : String
        importLines =
            imports
                |> List.map (\m -> "import " ++ m)
                |> String.join "\n"
    in
    "module InterpreterWrapper__ exposing (results)\n\n"
        ++ importLines
        ++ "\n\n\nresults : String\nresults =\n    "
        ++ expression
        ++ "\n"


{-| BFS through the module import graph to find all transitively needed modules.
-}
transitiveModuleDeps : Dict String (Set String) -> Set String -> Set String
transitiveModuleDeps importsGraph rootSet =
    bfsModules importsGraph (Set.toList rootSet) rootSet


bfsModules : Dict String (Set String) -> List String -> Set String -> Set String
bfsModules importsGraph queue visited =
    case queue of
        [] ->
            visited

        current :: rest ->
            let
                directDeps =
                    Dict.get current importsGraph
                        |> Maybe.withDefault Set.empty

                newDeps =
                    Set.diff directDeps visited

                newQueue =
                    rest ++ Set.toList newDeps

                newVisited =
                    Set.union visited newDeps
            in
            bfsModules importsGraph newQueue newVisited


{-| Topologically sort the needed modules using DFS post-order,
returning their sources in dependency order.
-}
topoSortModules : ModuleGraph -> Set String -> List String
topoSortModules graph needed =
    let
        dfs :
            String
            -> { visited : Set String, order : List String }
            -> { visited : Set String, order : List String }
        dfs moduleName acc =
            if Set.member moduleName acc.visited then
                acc

            else
                let
                    withVisited =
                        { acc | visited = Set.insert moduleName acc.visited }

                    deps =
                        Dict.get moduleName graph.imports
                            |> Maybe.withDefault Set.empty
                            |> Set.toList
                            |> List.filter (\dep -> Set.member dep needed)

                    afterDeps =
                        List.foldl dfs withVisited deps
                in
                case Dict.get moduleName graph.moduleToSource of
                    Just src ->
                        { afterDeps | order = afterDeps.order ++ [ src ] }

                    Nothing ->
                        afterDeps

        result =
            Set.toList needed
                |> List.foldl dfs { visited = Set.empty, order = [] }
    in
    result.order


reachableModules : ModuleGraph -> Set String -> Set String
reachableModules graph roots =
    let
        dfs : String -> Set String -> Set String
        dfs moduleName visited =
            if Set.member moduleName visited then
                visited

            else
                let
                    nextVisited =
                        Set.insert moduleName visited

                    deps =
                        Dict.get moduleName graph.imports
                            |> Maybe.withDefault Set.empty
                in
                deps
                    |> Set.toList
                    |> List.foldl dfs nextVisited
    in
    roots
        |> Set.toList
        |> List.foldl dfs Set.empty
