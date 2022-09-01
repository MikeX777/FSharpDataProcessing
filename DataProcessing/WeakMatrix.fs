namespace DataProcessing

module WeakMatrix =

    open FSharp.Stats
    open DataProcessing.Result

    type WeakMatrix<'T> =
        | TwoDimMatrix of List<List<'T>>
        | ColumnVector of List<'T>
        | RowVector of List<'T>

    let private validateTwoDimIsNotEmpty (toCreate:List<List<'T>>) : Result<List<List<'T>>, ErrorResponse> =
        if toCreate.IsEmpty then Error { ErrorResponse.Message = Some "No Rows found in Matrix." }
        else Success toCreate

    let private validateTwoDimFirstRowIsNotEmpty (toCreate:List<List<'T>>) : Result<List<List<'T>>, ErrorResponse> = 
        if toCreate.Head.IsEmpty then Error { ErrorResponse.Message = Some "First Row is empty." }
        else Success toCreate

    let private validateTwoDimAllRowsHaveSameColumns (toCreate:List<List<'T>>) : Result<List<List<'T>>, ErrorResponse> =
        let rowLength = toCreate.Head.Length
        if not (toCreate |> List.forall(fun row -> row.Length = rowLength)) then Error { ErrorResponse.Message = Some "Not all rows have the same length." }
        else Success toCreate

    let private validateTwoDimInput (toCreate:List<List<'T>>) : Result<List<List<'T>>, ErrorResponse> =
        toCreate
        |> validateTwoDimIsNotEmpty 
        >>= validateTwoDimFirstRowIsNotEmpty
        >>= validateTwoDimAllRowsHaveSameColumns

    let private validateOneDimIsNotEmpty (toCreate:List<'T>) : Result<List<'T>, ErrorResponse> =
        if toCreate.IsEmpty then Error { ErrorResponse.Message = Some "Matrix is empty." }
        else Success toCreate

    let private validateOneDimInput (toCreate:List<'T>) : Result<List<'T>, ErrorResponse> =
        toCreate
        |> validateOneDimIsNotEmpty

    let createTwoDimMatrix<'T> (toCreate:List<List<'T>>) =
        match validateTwoDimInput toCreate with 
            | Success s -> TwoDimMatrix s |> succeed
            | Error e -> Error e

    let createColumnMatix<'T> (toCreate:List<'T>) =
        match validateOneDimInput toCreate with
            | Success s -> ColumnVector s |> succeed
            | Error e -> Error e
            
    let createRowMatix<'T> (toCreate:List<'T>) =
        match validateOneDimInput toCreate with
            | Success s -> RowVector s |> succeed
            | Error e -> Error e

    let transpose matrix = 
        match matrix with
            | RowVector rm -> ColumnVector rm |> succeed
            | ColumnVector cm -> RowVector cm |> succeed
            | TwoDimMatrix tdm ->
                match tdm with
                | [] -> Error { ErrorResponse.Message = Some "How did you get here? No Empty 2 Dimentional Matricies allowed." }
                | x::xs ->
                  let rec loop matrix partial = 
                    match matrix with
                    | [] -> partial
                    | y::ys ->let newPartial = (y, partial) ||> List.map2(fun x y->x::y)
                              loop ys newPartial
                  let length = List.length x
                  loop tdm (List.init length (fun _ -> [] ))
                  |> List.map(fun x->x |> List.rev)
                  |> TwoDimMatrix
                  |> succeed
                    

    let rows matrix =
        match matrix with
            | RowVector rm -> RowVector rm |> succeed
            | ColumnVector cm -> Error { ErrorResponse.Message = Some "Rows do not exist on a column vector." }
            | TwoDimMatrix tdm -> TwoDimMatrix tdm |> succeed 

    let columns matrix =
        match matrix with
            | RowVector rm -> Error { ErrorResponse.Message = Some "Columns do not exist on a row vector." }
            | ColumnVector cm -> ColumnVector cm |> succeed
            | TwoDimMatrix tdm -> 
                transpose (TwoDimMatrix tdm)

