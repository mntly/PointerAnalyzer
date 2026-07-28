module EvaluateAnalyzer.Evaluator.Metric

open System.Text.Json
open EvaluateAnalyzer.Evaluator.Types

/// <summary>
/// Used for track the number of each case.
/// </summary>
/// <remarks>
/// <c>Total</c> indicates the total number of elements belong to
/// corrsesponding case.
/// <c>GTAddress</c> indicates the total number of elements belong to
/// corrsesponding case whose ground truth type is Address.
/// <c>GTValue</c> indicates the total number of elements belong to
/// corrsesponding case whose ground truth type is Value.
/// </remarks>
type CountBucket =
  { Total: int
    GTAddress: int
    GTValue: int }

/// <summary>
/// Track the metric of all cases.
/// </summary>
/// <remarks>
/// <c>All</c> indicates the total number of elements.
/// <c>Correct</c> tracks the correctly inferred elements.
/// <c>MisInferred</c> tracks the misinferred elements.
/// <c>Conflict</c> tracks the elements inferred as both Address and Value.
/// <c>Correct</c> tracks the elemtnes not be inferred.
/// </remarks>
type CountResult =
  { GTAll: int
    All: int
    Correct: CountBucket
    MisInferred: CountBucket
    Conflict: CountBucket
    Fail: CountBucket }

/// <summary>
/// Used for representing the ratio of each case.
/// </summary>
/// <remarks>
/// <c>Total</c> represents the ratio of the elements belong to corresponding
/// case among entire elements
/// <c>GTAddress</c> represents the ratio of the elements whose ground truth
/// type is Address among corresponding elements.
/// <c>GTValue</c> represents the ratio of the elements whose ground truth
/// type is Value among corresponding elements.
/// </remarks>
type RatioBucket =
  { Total: float
    GTAddress: float
    GTValue: float }

/// <summary>
/// Track the metric of all cases.
/// </summary>
/// <remarks>
/// <c>Correct</c> tracks the correctly inferred elements.
/// <c>MisInferred</c> tracks the misinferred elements.
/// <c>Conflict</c> tracks the elements inferred as both Address and Value.
/// <c>Correct</c> tracks the elemtnes not be inferred.
/// </remarks>
type RatioResult =
  { Correct: RatioBucket
    MisInferred: RatioBucket
    Conflict: RatioBucket
    Fail: RatioBucket }

/// <summary>
/// Represent the ratio per GT Type.
/// </summary>
/// <remarks>
/// <c>Correct</c> tracks the correctly inferred elements among corresponding
/// GT type.
/// <c>MisInferred</c> tracks the misinferred elements among corresponding
/// GT type.
/// <c>Conflict</c> tracks the elements inferred as both Address and Value
/// among corresponding GT type.
/// <c>Correct</c> tracks the elemtnes not be inferred among corresponding
/// GT type.
/// </remarks>
type GTTypeRatioBucket =
  { Correct: float
    MisInferred: float
    Conflict: float
    Fail: float }

/// <summary>
/// Represent the ratio per GT Type.
/// </summary>
type GTTypeRatioResult =
  { GTAddress: GTTypeRatioBucket
    GTValue: GTTypeRatioBucket }

/// <summary>
/// Represent confusion metrix of final evaluation.
/// Unknown inferred type is treated as Value when calculating this metric.
/// Conflict inferred type is treated as both Address and Value when
/// calculating this metric.
/// </summary>
type ConfusionMetrix = { TP: int; TN: int; FP: int; FN: int }

/// <summary>
/// Represent final binary classification metrics.
/// Unknown inferred type is treated as Value when calculating this metric.
/// Conflict inferred type is treated as both Address and Value when
/// calculating this metric.
/// </summary>
/// <remarks>
/// <c>Acc</c> is accuracy.
/// <c>Recall</c> is recall when Address is treated as positive.
/// <c>Precision</c> is precision when Address is treated as positive.
/// </remarks>
type FinalResult =
  { Confusion: ConfusionMetrix
    Acc: float
    Recall: float
    Precision: float }

/// <summary>
/// Represent the metrics related to structure slot coverage will be stored as
/// JSON.
/// </summary>
type StructureSlotCoverage =
  { Elements: int
    FullyObserved: int
    PartiallyObserved: int
    Unobserved: int
    GTSlots: int
    ObservedSlots: int
    MissingSlots: int
    Ratio: float }

/// <summary>
/// Represent entire metrics that will be stored as JSON.
/// </summary>
type EvalMetric =
  { Count: CountResult
    Ratio: RatioResult
    GTTypeRatio: GTTypeRatioResult
    FinalResult: FinalResult
    StructureSlotCoverage: StructureSlotCoverage }

let private emptyBucket: CountBucket =
  { Total = 0
    GTAddress = 0
    GTValue = 0 }

/// According to GT type, adjust the number of GT type of given CountBucket
let private incrementBucket gt (bucket: CountBucket) : CountBucket =
  match gt with
  | Address ->
    { bucket with
        Total = bucket.Total + 1
        GTAddress = bucket.GTAddress + 1 }
  | Value ->
    { bucket with
        Total = bucket.Total + 1
        GTValue = bucket.GTValue + 1 }
  | _ -> bucket

/// Count the # of elements in each case
let buildCount gtAll results =
  let folder (count: CountResult) (result: ElementResult) =
    match result.Category with
    | Correct ->
      { count with
          Correct = incrementBucket result.GT count.Correct }
    | MisInferred ->
      { count with
          MisInferred = incrementBucket result.GT count.MisInferred }
    | ConflictResult ->
      { count with
          Conflict = incrementBucket result.GT count.Conflict }
    | Fail ->
      { count with
          Fail = incrementBucket result.GT count.Fail }

  let initial: CountResult =
    { GTAll = gtAll
      All = List.length results
      Correct = emptyBucket
      MisInferred = emptyBucket
      Conflict = emptyBucket
      Fail = emptyBucket }

  List.fold folder initial results

let private div numerator denominator =
  if denominator = 0 then
    0.0
  else
    float numerator / float denominator

/// Calculate ratio of given CountBucket.
/// `Total` ratio is calculated using given the # of all elements.
let private ratioBucket all (bucket: CountBucket) : RatioBucket =
  { Total = div bucket.Total all
    GTAddress = div bucket.GTAddress bucket.Total
    GTValue = div bucket.GTValue bucket.Total }

/// In final binary metric, Unknown inferred type is considered as Value.
let private normalizeFinalType typ =
  match typ with
  | Unknown -> Value
  | other -> other

/// Calculate final Accuracy/Recall/Precision. Address is positive.
let private buildFinalResult results =
  let folder (tp, tn, fp, fn) (result: ElementResult) =
    match result.GT, normalizeFinalType result.Inferred with
    | Address, Address -> tp + 1, tn, fp, fn
    | Value, Value -> tp, tn + 1, fp, fn
    | Value, Address -> tp, tn, fp + 1, fn
    | Address, Value -> tp, tn, fp, fn + 1
    | Address, Conflict -> tp, tn, fp, fn + 1
    | Value, Conflict -> tp, tn, fp + 1, fn
    | _ -> tp, tn, fp, fn

  let tp, tn, fp, fn = List.fold folder (0, 0, 0, 0) results
  let all = tp + tn + fp + fn

  { Confusion = { TP = tp; TN = tn; FP = fp; FN = fn }
    Acc = div (tp + tn) all
    Recall = div tp (tp + fn)
    Precision = div tp (tp + fp) }

/// Converge all structure slot metrics of each function
let private buildStructureSlotCoverage (coverages: StructureCoverage list) =
  let expected =
    coverages |> List.sumBy (fun coverage -> coverage.ExpectedSlots)

  let observed =
    coverages |> List.sumBy (fun coverage -> coverage.ObservedSlots)

  let countHelper f =
    coverages |> List.filter f |> List.length

  let fullyObserved =
    countHelper (fun coverage ->
      coverage.ObservedSlots = coverage.ExpectedSlots)

  let partialObserved =
    countHelper (fun coverage ->
      coverage.ObservedSlots > 0
      && coverage.ObservedSlots < coverage.ExpectedSlots)

  let unObserved = countHelper (fun coverage -> coverage.ObservedSlots = 0)

  { Elements = List.length coverages
    FullyObserved = fullyObserved
    PartiallyObserved = partialObserved
    Unobserved = unObserved
    GTSlots = expected
    ObservedSlots = observed
    MissingSlots = expected - observed
    Ratio = div observed expected }

/// Calculate evaluation metrix using the result of evaluation classification
let build gtAll results structureCoverages =
  (* Count the # of elements in each case *)
  let count: CountResult = buildCount gtAll results

  (* Calculate ratio of each case *)
  let ratio: RatioResult =
    { Correct = ratioBucket count.All count.Correct
      MisInferred = ratioBucket count.All count.MisInferred
      Conflict = ratioBucket count.All count.Conflict
      Fail = ratioBucket count.All count.Fail }

  (* Count the # of elements whose GT type is Address *)
  (* This will be used when calculating metric per GT Type *)
  let addressTotal =
    count.Correct.GTAddress
    + count.MisInferred.GTAddress
    + count.Conflict.GTAddress
    + count.Fail.GTAddress

  (* Count the # of elements whose GT type is Value *)
  (* This will be used when calculating metric per GT Type *)
  let valueTotal =
    count.Correct.GTValue
    + count.MisInferred.GTValue
    + count.Conflict.GTValue
    + count.Fail.GTValue

  (* Calculate metric per GT Address *)
  let addressRatio: GTTypeRatioBucket =
    { Correct = div count.Correct.GTAddress addressTotal
      MisInferred = div count.MisInferred.GTAddress addressTotal
      Conflict = div count.Conflict.GTAddress addressTotal
      Fail = div count.Fail.GTAddress addressTotal }

  (* Calculate metric per GT Value *)
  let valueRatio: GTTypeRatioBucket =
    { Correct = div count.Correct.GTValue valueTotal
      MisInferred = div count.MisInferred.GTValue valueTotal
      Conflict = div count.Conflict.GTValue valueTotal
      Fail = div count.Fail.GTValue valueTotal }

  { Count = count
    Ratio = ratio
    GTTypeRatio =
      { GTAddress = addressRatio
        GTValue = valueRatio }
    FinalResult = buildFinalResult results
    StructureSlotCoverage = buildStructureSlotCoverage structureCoverages }

/// Transform given metric DS to JSON
let toJson metric =
  let options = JsonSerializerOptions (WriteIndented = true)
  JsonSerializer.Serialize (metric, options)
