
open DataProcessing.Result
open DataProcessing.CsvProcessing
open System.IO
open System
open Assignment1.Income.IncomeModeling
open Assignment1.Income.DataPrinter

// For more information see https://aka.ms/fsharp-console-apps


let baseDirectory = __SOURCE_DIRECTORY__
let baseDirectory' = Directory.GetParent(baseDirectory)
let filePath = "Data\Credit_N400_p9.csv"
let fullPath = Path.Combine(baseDirectory, filePath)
let outputPath = Path.Combine(baseDirectory, "output.txt")

let parsed = parseFile false 1500 true ',' fullPath
let cvTest = parsed >>= foldedPreProcess 20

match cvTest with 
| Error e -> printfn "Error while processing the data"
| Success d ->
    let (headers, folds) = d
    use file = File.OpenWrite(outputPath)
    folds |> List.iteri (fun i fold ->
        file.Write(System.Text.Encoding.UTF8.GetBytes(printFold i fold)))
    file.Close()

printfn "All done!"
