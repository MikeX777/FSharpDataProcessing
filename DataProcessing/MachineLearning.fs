namespace DataProcessing

open Result
open WeakMatrix

module MachineLearning =

    let inline private average values = 
        List.fold (fun acc value -> acc + float value) 0.0 values / (float values.Length)
        
    let inline private standardDeviation values =
        let average = average values
        sqrt (List.fold (fun acc value -> acc + ((float value - average) ** 2)) 0.0 values / (float values.Length))
        
    let inline private divideByStandardDeviation values =
        let standardDeviation = standardDeviation values
        values |> List.map (fun f ->  f / standardDeviation) |> succeed

    let inline centerList values =
        let average = average values
        values |> List.map (fun i -> float i - average) |> succeed

    let inline standardizeList values =
        values
        |> centerList
        >>= divideByStandardDeviation

