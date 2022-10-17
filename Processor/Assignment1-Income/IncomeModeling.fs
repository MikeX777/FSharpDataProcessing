namespace Assignment1.Income

open DataProcessing.Result
open DataProcessing.WeakMatrix
open DataProcessing.MachineLearning

module IncomeModeling =

    let private transformCategories list =
        List.map (fun string ->
            if (string = "Male" || string = "No") then 0.0
            elif (string = "Female" || string = "Yes") then 1.0
            else System.Double.Parse(string)) list

    let inline private transformData data = 
        let (headers, lines) = data
        let transformed = lines |> List.map (fun x -> transformCategories x)
        (headers, transformed) |> succeed

    let inline getLastItemInList list =
        let rec loop acc l =
            match l with
            | [] -> ([], [])
            | h::t ->
                if List.length t = 1 then
                    (List.append acc [h], t)
                else
                    loop (List.append acc [h]) t
        loop [] list


    let inline private makeSlice acc (lines:List<List<float>>) startValidationIndex endValidationIndex =
        if (startValidationIndex = 0) then
            List.append acc [(lines[endValidationIndex+1..lines.Length-1],lines[startValidationIndex..endValidationIndex])]
        elif (endValidationIndex = lines.Length-1) then
            List.append acc [(lines[0..startValidationIndex-1],lines[startValidationIndex..endValidationIndex])]
        else
            List.append acc [(List.append lines[0..startValidationIndex-1] lines[endValidationIndex+1..lines.Length-1],lines[startValidationIndex..endValidationIndex])]

    let inline private makeFolds validationPercentage (data:(List<string> * List<List<float>>)) : Result<(List<string> * List<(List<List<float>> * List<List<float>>)>), ErrorResponse>  =
        let (headers, lines) = data
        
        if (validationPercentage < 0) then
            { ErrorResponse.Message = Some "Cannot have a negative validation percentage." } |> fail
        elif (validationPercentage = 0) then
            (headers, [(lines, [])]) |> succeed
        elif ((float 100) / (float validationPercentage) % (float 1) <> 0.0) then
            { ErrorResponse.Message = Some "Fold percentage does not evenly divide dataset." } |> fail
        else
            let linesToSlice = int ((float lines.Length) * (float validationPercentage / float 100))
            let slicesToMake = lines.Length / linesToSlice
            let (result, slices) = ([], [1..slicesToMake]) ||> List.mapFold (fun acc sliceIndex ->
                let startValidationIndex = if (sliceIndex = slicesToMake) then 0 else lines.Length - (sliceIndex * linesToSlice)
                let endValidationIndex = startValidationIndex + linesToSlice - 1
                (sliceIndex,makeSlice acc lines startValidationIndex endValidationIndex))

            (headers, slices) |> succeed

    let private extractOutputs matrix =
        match matrix with
        | TwoDimMatrix l ->
            let (inputs, outputs) = getLastItemInList l
            ((createTwoDimMatrix inputs), (createColumnMatrix outputs[0])) |> succeed
        | _ -> { ErrorResponse.Message = Some "Error, matrix was not able a Two dimensional matrix. Could not extract outputs." } |> fail

    let private makeData inputAndOutputs =
        match inputAndOutputs with
        | (Success i, Success o) ->
            Data (i,o) |> succeed
        | (_,_) -> { ErrorResponse.Message = Some "Could not make data. Error before processings the data." } |> fail

    let separateOutputs list =
        match list with
        | [] -> EmptyData ([], []) |> succeed
        | _ ->
            list
            |> createTwoDimMatrix
            >>= columns
            >>= extractOutputs
            >>= makeData

    let separateFeaturesFolded data =
        let (headers, slices) = data
        let separated = slices |> List.map(fun s ->
            let (training, validation) = s
            let trainingData = separateOutputs training
            let validationData = separateOutputs validation
            match (trainingData,validationData) with
            | (Success t, Success v) ->
                { DataGrouping.Training = t; DataGrouping.Validation = v } |> succeed
            | (_, _) -> { ErrorResponse.Message = Some "Error while separating folded features." } |> fail)
        if not (separated |> List.forall (fun i -> isSuccess i)) then
            { ErrorResponse.Message = Some "Error creating data from separating the folded feature." } |> fail
        else
            let unwrappedData = ([], separated) ||> List.fold (fun acc i ->
                match i with
                | Success s -> List.append acc [s]
                | Error _ -> acc)
            (headers,unwrappedData) |> succeed

    let inline private centerAndStandardizeFoldWithValidation trainInputs trainOutputs validationInputs validationOutputs =
        let outputAverage = average trainOutputs
        let inputAveragesAndDeviation = trainInputs |> List.fold (fun acc i -> List.append acc [(average i, standardDeviation (centerList i))]) []
        let processedTrainedOutputs = trainOutputs |> List.map (fun x -> x - outputAverage)
        let processedValidationOutputs = validationOutputs |> List.map (fun x -> x - outputAverage)
        let processedTrainedInputs = createTwoDimMatrix ((trainInputs, inputAveragesAndDeviation) ||> List.map2 (fun i aAndS ->
            let (average, standardDeviation) = aAndS
            stadardizeListWithAverageAndDeviation average standardDeviation i)) >>= transpose
        let processedValidationInputs = createTwoDimMatrix ((validationInputs, inputAveragesAndDeviation) ||> List.map2 (fun i aAndS ->
            let (average, standardDeviation) = aAndS
            stadardizeListWithAverageAndDeviation average standardDeviation i)) >>= transpose

        match (processedTrainedInputs,processedValidationInputs) with
        | (Success ti, Success vi) ->
            { Training = Data (ti, (ColumnVector processedTrainedOutputs)); Validation = Data (vi, (ColumnVector processedValidationOutputs)) } |> succeed
        | (_, _) -> { ErrorResponse.Message = Some "Error while centering and standardizing fold with a validation set." } |> fail


    let inline private centerAndStandardizeFold trainInputs trainOutputs =
        let inputs = (createTwoDimMatrix (trainInputs |> List.map (fun x -> standardizeList x))) >>= transpose
        match inputs with
        | Success s ->
            { Training = Data (s, (ColumnVector (centerList trainOutputs))); Validation = EmptyData ([],[]) } |> succeed
        | Error _ -> { ErrorResponse.Message = Some "Error while processing the fold" } |> fail

    let inline private processFold fold =
        match (fold.Training, fold.Validation) with
        | (Data (TwoDimMatrix trainInputs, ColumnVector trainOutputs), Data (TwoDimMatrix validationInputs, ColumnVector validationOutputs)) ->
            centerAndStandardizeFoldWithValidation trainInputs trainOutputs validationInputs validationOutputs
        | (Data (TwoDimMatrix trainInputs, ColumnVector trainOutputs), EmptyData _) ->
            centerAndStandardizeFold trainInputs trainOutputs
        | (_, _) -> { ErrorResponse.Message = Some "Fold did not have the proper shape for processFold" } |> fail

    let inline private processFoldedInputsAndOutputs data =
        let (headers, folds) = data
        let processed = folds |> List.map (fun fold -> processFold fold)
        if not (processed |> List.forall (fun f -> isSuccess f)) then
            { ErrorResponse.Message = Some "Error while processing the folds." } |> fail
        else
            let unwrappedFolds = ([], processed) ||> List.fold (fun acc i ->
                match i with
                | Success s -> List.append acc [s]
                | Error _ -> acc)
            (headers, unwrappedFolds) |> succeed
        
    let foldedPreProcess validationPercentage data =
        data
        |> transformData
        >>= makeFolds validationPercentage
        >>= separateFeaturesFolded
        >>= processFoldedInputsAndOutputs


//#region Original Data Process Without Folds

    let unWrapSuccess x =
        match x with 
        | Success s -> s
        | Error e -> []

    let inline private loopInputs inputs func =
        let rec loop list acc =
            match list with
            | head :: tail -> 
                let processed = func head
                match processed with
                | [] -> loop tail acc
                | _ -> loop tail (List.append acc [processed])
            | [] -> acc
        loop inputs List.empty

    let combineHeaders headers tuple =
        let (inputs, outputs) = tuple
        (headers, inputs, outputs) |> succeed

    let separateFeatures data =
        let (headers, lines) = data
        lines |> 
        createTwoDimMatrix
        >>= columns
        >>= extractOutputs
        >>= combineHeaders headers

    let private processInputsAndOutputs data =
        let (headers, inputs, outputs) = data
        match (inputs, outputs) with
        | (Success (TwoDimMatrix i), Success (ColumnVector o)) ->
            let centeredOutputs = o |> centerListResult
            let standardizedInputs = loopInputs i (transformCategories >> standardizeListResult >> unWrapSuccess)
            match centeredOutputs with
            | Success o -> (headers, (createTwoDimMatrix standardizedInputs >>= transpose), (createColumnMatrix o)) |> succeed
            | _ -> { ErrorResponse.Message = Some "Error processingInputsAndOutputs" } |> fail
        | (_, _) -> { ErrorResponse.Message = Some "The inputs were not proper for this application." } |> fail

    let preprocessData  data = 
        data 
        |> separateFeatures
        >>= processInputsAndOutputs

//#endregion
