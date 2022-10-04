namespace DataProcessing

open Result
open WeakMatrix

module MachineLearning =

    let inline average values = 
        List.fold (fun acc value -> acc + float value) 0.0 values / (float values.Length)
        
    let inline standardDeviation values =
        let average = average values
        sqrt (List.fold (fun acc value -> acc + ((float value - average) ** 2)) 0.0 values / (float values.Length))
        
    let inline private divideByStandardDeviation values =
        let standardDeviation = standardDeviation values
        values |> List.map (fun f ->  f / standardDeviation)

    let inline private divideByProvidedStandardDeviation std values =
        values |> List.map (fun f -> f / std)

    let inline centerList values =
        let average = average values
        values |> List.map (fun i -> float i - average)

    let inline centerListWithAverage average values =
        values |> List.map (fun i -> float i - average)

    let inline standardizeList values =
        values
        |> (centerList >> divideByStandardDeviation)

    let inline stadardizeListWithAverageAndDeviation average std values =
        values
        |> centerListWithAverage average
        |> divideByProvidedStandardDeviation std

    let inline private divideByStandardDeviationResult values =
        let standardDeviation = standardDeviation values
        values |> List.map (fun f ->  f / standardDeviation) |> succeed
        
    let inline centerListResult values =
        let average = average values
        values |> List.map (fun i -> float i - average) |> succeed

    let inline standardizeListResult values =
        values
        |> centerListResult
        >>= divideByStandardDeviationResult

