
open DataProcessing.Result
open DataProcessing.CsvProcessing
open System.IO
open System
open Assignment1.Income.IncomeModeling

// For more information see https://aka.ms/fsharp-console-apps


let baseDirectory = __SOURCE_DIRECTORY__
let baseDirectory' = Directory.GetParent(baseDirectory)
let filePath = "Data\Credit_N400_p9.csv"
let fullPath = Path.Combine(baseDirectory, filePath)

let parsed = parseFile true ',' fullPath
let processed = parsed >>= preprocessData

printfn "%A" processed