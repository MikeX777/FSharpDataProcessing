namespace DataProcessing

open System
open Result
open WeakMatrix

module MachineLearning =

    type Data<'T> =
        | Data of WeakMatrix<'T> * WeakMatrix<'T>
        | EmptyData of List<List<'T>> * List<List<'T>>

    type DataGrouping<'T> =
        {
        Training: Data<'T>
        Validation: Data<'T>
        }

    type LearnedData =
        {
        Weights: WeakMatrix<float>
        MSE: float
        }

    let inline private getValues (rand:System.Random) (minValue, maxValue) =
        Seq.initInfinite (fun _ -> rand.Next(minValue, maxValue))

    let inline average values = 
        List.fold (fun acc value -> acc + float value) 0.0 values / (float values.Length)
        
    let inline standardDeviation values =
        let average = average values
        sqrt (List.fold (fun acc value -> acc + ((float value - average) ** 2)) 0.0 values / (float values.Length))

    let inline normalizeList list =
        list |> List.map (fun x -> (float 2) * (((float x) - float (List.min list))/(float (List.max list) - float (List.min list))) - (float 1))
        
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

    let transposeAndMultiply inputs toMultiply =
        match transpose inputs with
        | Success transposed -> slowMatrixMultiplication transposed toMultiply
        | Error e -> e |> fail

    let scalarMultiplyAndSubtract parameters lambda toSubtract =
        match scalarMultiplication parameters lambda with
        | Success scaled -> matrixSubtraction scaled toSubtract
        | Error e -> e |> fail

    let inline ridgeRegressionIteration parameters alpha lambda inputs outputs =
        parameters
        |> slowMatrixMultiplication inputs
        >>= matrixSubtraction outputs
        >>= transposeAndMultiply inputs
        >>= scalarMultiplyAndSubtract parameters lambda
        >>= reverseScalarMultiplication (float 2 * alpha)
        >>= matrixSubtraction parameters

    let inline runTraining iterations parameters alpha lambda trainingInputs trainingOutputs =
        let rec regressionLoop iteration previousParamters newParameters alpha lambda inputs outputs =
            match (previousParamters, newParameters) with
            | (Success previous, Success current) ->
                if iteration = iterations || previous = current then
                    { Weights = previous; MSE = 0.0 } |> succeed
                else
                    let nextIteration = ridgeRegressionIteration current alpha lambda inputs outputs
                    regressionLoop (iteration + 1) newParameters nextIteration alpha lambda inputs outputs
            | (_, _) -> { ErrorResponse.Message = Some "Error whlie training." } |> fail

        let firstIteration = ridgeRegressionIteration parameters alpha lambda trainingInputs trainingOutputs
        match firstIteration with
        | Success first ->
            let secondIteration = ridgeRegressionIteration first alpha lambda trainingInputs trainingOutputs
            regressionLoop 1 firstIteration secondIteration alpha lambda trainingInputs trainingOutputs
        | Error e -> { ErrorResponse.Message = Some $"Error after first iteration of train. Message: {e.Message}" } |> fail
        
    let inline runValidation validationInputs validationOutputs trained =
        match trained.Weights with
        | ColumnVector cv ->
            { Weights = trained.Weights; MSE = ((List.sum ((validationInputs, validationOutputs) ||> List.map2 (fun i o ->
                Math.Pow((o - List.sum ((i, cv) ||> List.map2 (fun input weight ->
                    input * weight))), 2)))) / float validationOutputs.Length) } |> succeed
        | _ -> { ErrorResponse.Message = Some "Error, given the wrong type of trained matrix for validation." } |> fail


    let inline trainFoldAndValidate iterations parameters alpha lambda trainingInputs trainingOutputs validationInputs validationOutputs =
        (runTraining iterations parameters alpha lambda trainingInputs trainingOutputs)
        >>= runValidation validationInputs validationOutputs

    let inline trainFold iterations parameters alpha lambda fold =
        match (fold.Training,fold.Validation) with
        | (Data (TwoDimMatrix it, ColumnVector ot), Data (TwoDimMatrix iv, ColumnVector ov)) -> trainFoldAndValidate iterations parameters alpha lambda (TwoDimMatrix it) (ColumnVector ot) iv ov
        | (Data (TwoDimMatrix it, ColumnVector ot), EmptyData _) -> runTraining iterations parameters alpha lambda (TwoDimMatrix it) (ColumnVector ot)
        | (_, _) -> { ErrorResponse.Message = Some "Error, fold was of wrong shape." } |> fail

    let inline train seed iterations alpha lambda (processedData:(string list * DataGrouping<float> list)) =
        let r = System.Random(seed)
        let (headers, folds) = processedData
        let nums = getValues r (1, 568420) |> Seq.take (headers.Length - 1)
        let parameters = createColumnMatrix (normalizeList (Seq.toList nums))

        match parameters with
        | Success beta ->
            let rec iter folds acc =
                match folds with
                | [] -> acc
                | h::t ->
                    match (trainFold iterations beta alpha lambda h) with
                    | Success trained ->
                        iter t (List.append acc [trained])
                    | Error e -> []

            iter folds [] |> succeed
        | _ -> { ErrorResponse.Message = Some "Error while creating parameter array." } |> fail

    let inline getCrossValidation learnedData =
        (learnedData |> List.sumBy (fun l -> l.MSE)) / float learnedData.Length



        

