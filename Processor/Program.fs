
open DataProcessing.Result
open DataProcessing.CsvProcessing
open DataProcessing.MachineLearning
open System.IO
open Assignment1.Income.IncomeModeling
open Assignment1.Income.DataPrinter

// For more information see https://aka.ms/fsharp-console-apps

// CONSTANTS
let shuffleLines = true
let seed = 1500
let hasHeader = true
let delimeterChar = ','
let validationPercentage = 20
let alpha = 0.00001
let lambdas = [0.01; 0.1; 1.0; 10.0; 100.0; 1000.0; 10000.0]
let idealLambda = 0.01
let iterations = 100000

printfn "*****************************CONSTANTS*****************************"
printfn "* suffleLines: %b                                               *" shuffleLines
printfn "* seed: %i                                                      *" seed
printfn "* hasHeader: %b                                                 *" hasHeader
printfn "* delimeterChar: %c                                                *" delimeterChar
printfn "* validationPercentage: %i                                        *" validationPercentage
printfn "* alpha %f                                                  *" alpha
printfn "* lambdas: %A         *" lambdas
printfn "* idealLambda: %f                                           *" idealLambda
printfn "* iterations: %i                                              *" iterations
printfn "*******************************************************************"

printfn "\n\n\n\n"


let baseDirectory = System.Environment.CurrentDirectory
let filePath = "Data\Credit_N400_p9.csv"
let fullPath = Path.Combine(baseDirectory, filePath)
let outputPath = Path.Combine(baseDirectory, "output.csv")

printfn "Parsing the data file."
let parsed = parseFile shuffleLines seed hasHeader delimeterChar fullPath

printfn "Processing the data for unfolded training."
let unfolded = parsed >>= foldedPreProcess 0

printfn "Processing the data for a %i%% validation set." validationPercentage
let folded = parsed >>= foldedPreProcess validationPercentage

match (unfolded,folded) with
| (Error e1, Error e2) -> printfn $"Errors while processing data. Error 1: {e1} Error 2: {e2}"
| (_, Error e2) -> printfn $"Error while processing data. Error: {e2}"
| (Error e1, _) -> printfn $"Error while processing data. Error: {e1}"
| (Success u, Success f) ->
    let (headers, data) = u

    let (inputHeaders, outputHeader) = getLastItemInList headers

    printfn "Training Deliverable 1!"
    let trainedUnfolded = lambdas |> List.map (fun l -> train seed iterations alpha l u)

    printfn "Training Deliverable 2!"
    let trainedFolded = lambdas |> List.map (fun l -> train seed iterations alpha l f)

    printfn "Training Deliverable 4!"
    let trainUnfoldedIdealScaling = train seed iterations alpha idealLambda u
    
    let deliverable1Start = printDeliverable1Header + printHeaders inputHeaders
    let deliverable1Data = (lambdas, trainedUnfolded) ||> List.map2 (fun l u ->
        match u with
        | Success weights -> unwrapAndPrintWeights l weights.Head.Weights
        | Error e -> $"Error with lambda: {l}") |> List.toSeq

    let deliverable1Text = deliverable1Start + (String.concat "" deliverable1Data) + printEmptyLines

    let deliverable2Start = printDeliverable2Header + printDeliverable2DataHeader
    let deliverable2Data = (lambdas, trainedFolded) ||> List.map2 (fun l f ->
        match f with
        | Success fold -> printDeliverable2DataLine l (getCrossValidation fold)
        | Error e -> $"Error with lamdba: {l}") |> List.toSeq
    let deliverable2Text = deliverable2Start + (String.concat "" deliverable2Data) + printEmptyLines

    let deliverable4Start = printDeliverable4Header + printHeaders inputHeaders
    let deliverable4Data =  match trainUnfoldedIdealScaling with
                            | Success weights -> unwrapAndPrintWeights idealLambda weights.Head.Weights
                            | Error e -> $"Error while training Deliverable4: {e}"
    let deliverable4Text = deliverable4Start + deliverable4Data + printEmptyLines


    use file = File.OpenWrite(outputPath)
    file.Write(System.Text.Encoding.UTF8.GetBytes(deliverable1Text + deliverable2Text + deliverable4Text))
    file.Write(System.Text.Encoding.UTF8.GetBytes(sprintf "\n\n\n"))
    file.Close()

printfn "All done!"
