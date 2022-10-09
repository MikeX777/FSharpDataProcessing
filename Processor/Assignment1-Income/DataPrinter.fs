namespace Assignment1.Income

open DataProcessing.Result
open DataProcessing.WeakMatrix
open DataProcessing.MachineLearning
open IncomeModeling

module DataPrinter =

    let inline mapToCommaSeparatedSeq toMap =
        toMap |> List.map(fun v -> $"{v},") |> List.toSeq

    let printDeliverable1Header = "Deliverable1\n"

    let printWeights lambda weights =
        $"{lambda}," + String.concat "" (mapToCommaSeparatedSeq weights) + "\n"

    let unwrapAndPrintWeights lambda weights =
        match weights with
        | ColumnVector cv -> printWeights lambda cv
        | RowVector rv -> printWeights lambda rv
        | _ -> "Error printing Weights"

    let printHeaders headers =
        "Lambda," + String.concat "" (mapToCommaSeparatedSeq headers) + "\n"

    let printDeliverable1Data headers lambdas weights =
        let data = (lambdas, weights) ||> List.map2 (fun l w ->
            printWeights l w) |> List.toSeq
        (printHeaders headers) + String.concat "" data

    let printDeliverable1 headers lambdas weights = 
        printDeliverable1Header + (printDeliverable1Data headers lambdas weights)

    let printEmptyLines = "\n\n\n\n\n"

    let printDeliverable2Header = "Deliverable2\n"
    let printDeliverable2DataHeader = "Lambda,CrossValidation\n"

    let printDeliverable2DataLine lambda crossValidation =
        $"{lambda},{crossValidation}\n"

    let printDeliverable2Data lambdas crossValidations =
        let mapped = (lambdas, crossValidations) ||> List.map2 (fun l c ->
            $"{l},{c}\n") |> List.toSeq
        "Lambda,CrossValidation\n" + String.concat "" mapped

    let printDeliverable2 lambdas crossValidations =
        printDeliverable2Header + (printDeliverable2Data lambdas crossValidations)

    let printDeliverable4Header = "Deliverable4\n"

    let inline printDeliverables lambdas headers weights crossValidations =
        printDeliverable1 headers lambdas weights
        + printEmptyLines
        + printDeliverable2 lambdas crossValidations

    let inline printResult printFunc value =
        match value with
        | Success s -> printFunc s
        | Error e -> $"Error before printing: {e.Message}\n"

    

    let inline printRow row =
        sprintf "%A\n" row

    let inline printMatrix matrix =
        match matrix with
        | RowVector rv -> printRow rv
        | ColumnVector cv -> String.concat "" (cv |> Seq.map (fun c -> printRow c))
        | TwoDimMatrix m -> String.concat "" (m |> Seq.map (fun r -> printRow r))

    let inline printResultMatrix result =
        match result with
        | Success matrix -> printMatrix matrix
        | Error e -> $"There was an error with the result matrix: {e.Message}"


    let inline printData data =
        match data with
        | Data (inputs, outputs) -> "Inputs:\n" + printMatrix inputs + "\n\n\nOutputs:\n" + printMatrix outputs + "\n\n\n"
        | EmptyData _ -> "EmptyData!\n\n\n"

    let inline printDataGrouping data =
        "Training Data:\n" + printData data.Training + "Validation Data:\n" + printData data.Validation

    let inline printLearnedData learnedData =
        sprintf "Weights: %A\nMSE: %A\n\n\n" learnedData.Weights learnedData.MSE 

    let inline printScaledLearnedData lambda foldNumber learnedData =
        sprintf "This is lambda %f foldNumber %i\n\n" lambda foldNumber + printLearnedData learnedData

    let inline printFold iteration fold =
        sprintf "Fold Iteration %i:\n" iteration + printDataGrouping fold