namespace Assignment1.Income

open DataProcessing.Result
open DataProcessing.WeakMatrix
open DataProcessing.MachineLearning

module IncomeModeling =
    
    type Gender =
        | Male of string
        | Female of string

    type Observation =
        {
        CreditLimit: int
        Rating: int
        NumOfCards: int
        Education: int
        Gender: Gender
        Student: bool
        Married: bool
        Balance: int
        Income: float
        }

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

    let private transformCategories column =
        List.map (fun string ->
            if (string = "Male" || string = "No") then 0.0
            elif (string = "Female" || string = "Yes") then 1.0
            else System.Double.Parse(string)) column

    let private extractOutputs matrix =
        match matrix with
        | TwoDimMatrix l -> ((createTwoDimMatrix l.Tail), (createColumnMatrix l.Head)) |> succeed
        | _ -> { ErrorResponse.Message = Some "Error, matrix was not able a Two dimensional matrix. Could not extract outputs." } |> fail

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

    let unWrapSuccess x =
        match x with 
        | Success s -> s
        | Error e -> []

    let private processInputsAndOutputs data =
        let (headers, inputs, outputs) = data
        match (inputs, outputs) with
        | (Success (TwoDimMatrix i), Success (ColumnVector o)) ->
            let centeredOutputs = o |> centerList
            let standardizedInputs = loopInputs i (transformCategories >> standardizeList >> unWrapSuccess)
            match centeredOutputs with
            | Success o -> (headers, (createTwoDimMatrix standardizedInputs >>= transpose), (createColumnMatrix o)) |> succeed
            | _ -> { ErrorResponse.Message = Some "Error processingInputsAndOutputs" } |> fail
        | (_, _) -> { ErrorResponse.Message = Some "The inputs were not proper for this application." } |> fail     

    let preprocessData data = 
        data 
        |> separateFeatures
        >>= processInputsAndOutputs


