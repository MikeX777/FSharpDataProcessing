namespace Assignment1.Income

open DataProcessing.Result
open DataProcessing.WeakMatrix
open DataProcessing.MachineLearning
open IncomeModeling

module DataPrinter =

    let inline printRow row =
        sprintf "%A\n" row

    let inline printMatrix matrix =
        match matrix with
        | RowVector rv -> printRow rv
        | ColumnVector cv -> String.concat "" (cv |> Seq.map (fun c -> printRow c))
        | TwoDimMatrix m -> String.concat "" (m |> Seq.map (fun r -> printRow r))

    let inline printData data =
        match data with
        | Data (inputs, outputs) -> "Inputs:\n" + printMatrix inputs + "\n\n\nOutputs:\n" + printMatrix outputs + "\n\n\n"
        | EmptyData _ -> "EmptyData!\n\n\n"

    let inline printDataGrouping data =
        "Training Data:\n" + printData data.Training + "Validation Data:\n" + printData data.Validation

    let inline printFold iteration fold =
        sprintf "Fold Iteration %i:\n" iteration + printDataGrouping fold