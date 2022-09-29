namespace DataProcessing

module WeakMatrix =

    open DataProcessing.Result

    type WeakMatrix<'T> =
        | TwoDimMatrix of List<List<'T>>
        | ColumnVector of List<'T>
        | RowVector of List<'T>

    type RowMultiplicationRecord<'T> =
        {
        Vector: WeakMatrix<'T> option
        Matrix: WeakMatrix<'T> option
        }

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

    let createColumnMatrix<'T> (toCreate:List<'T>) =
        match validateOneDimInput toCreate with
            | Success s -> ColumnVector s |> succeed
            | Error e -> Error e
            
    let createRowMatrix<'T> (toCreate:List<'T>) =
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
                    | y::ys ->
                        let newPartial = (y, partial) ||> List.map2(fun x y->x::y)
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

    let inline multiplyColumnVectorByMatrix vector matrix =
        match (vector,matrix) with
        | (ColumnVector cv, TwoDimMatrix m) ->
            if cv.Length <> m.Head.Length then Error { ErrorResponse.Message = Some "Vector and matrix cannot be multiplied." }
            else
                let rec loopMatrix acc matrix vector =
                    match matrix with
                    | [] -> acc
                    | h::t -> loopMatrix (List.append acc [((List.zip h vector) |> List.sumBy (fun (a,b) -> a * b))]) t vector
                loopMatrix [] m cv
                |> createColumnMatrix
        | (_, _) -> Error { ErrorResponse.Message = Some "Not a supported vector and matrix for this function." }

    let inline private transposeRowOfRowMultipliaction vector =
        match (vector |> transpose) with
        | Success v -> { Vector = Some v; Matrix = None } |> succeed
        | Error e -> Error { ErrorResponse.Message = Some $"Multiplication failed on Row Transpose with message: {e.Message}" }

    let inline private transposeMatrixOfRowMultiplication matrix record =
        match (matrix |> transpose) with
        | Success m -> { Vector = record.Vector; Matrix = Some m } |> succeed
        | Error e -> Error { ErrorResponse.Message = Some $"Mulitplication failed on Matrix Transpose with message: {e.Message}" }

    let inline private unWrapRowMultiplicationRecord record =
        match (record.Vector,record.Matrix) with
        | (Some (ColumnVector c), Some (TwoDimMatrix m)) -> multiplyColumnVectorByMatrix (ColumnVector c) (TwoDimMatrix m)
        | (_, _) -> Error { ErrorResponse.Message = Some "Error unwrapping record, passed invaild valis for column vector multiplication." }

    let inline multiplyRowVectorByMatrix vector matrix =
        match (vector,matrix) with
        | (RowVector rv, TwoDimMatrix m) ->
            RowVector rv
            |> transposeRowOfRowMultipliaction
            >>= transposeMatrixOfRowMultiplication matrix
            >>= unWrapRowMultiplicationRecord
            >>= transpose
        | (_, _) -> Error { ErrorResponse.Message = Some "Not a supported vector and matrix for this function." }

    let inline slowTwoDimMatrixMultiplication matrix1 matrix2 =
        match (matrix1,matrix2) with
        | (TwoDimMatrix m1, TwoDimMatrix m2) ->
            if (m1.Head.Length = m2.Length) then
                let m2Columns = columns (TwoDimMatrix m2)
                match m2Columns with
                | Error e -> { ErrorResponse.Message = Some $"Error while trying to extrace the columns for the second matrix. Message: {e.Message}" } |> fail
                | Success (TwoDimMatrix m2c) ->
                    let rec columnLoop acc2 baseMatrix multiplier =
                        match baseMatrix with
                        | [] -> acc2
                        | h :: t -> columnLoop (List.append acc2 [(List.zip h multiplier) |> List.sumBy (fun (a,b) -> (a * b))]) t multiplier
                    let rec multiLoop acc multiplingMatrix =
                        match multiplingMatrix with
                        | [] -> acc
                        | h :: t -> multiLoop (List.append acc [columnLoop [] m1 h]) t
                    multiLoop [] m2c 
                    |> createTwoDimMatrix
                    >>= transpose
                | _ -> { ErrorResponse.Message = Some "Error, improper matricies made it to the slow two dimension multiplication function." } |> fail
            else
                { ErrorResponse.Message = Some "The dimensions of the matricies do not match for multiplication." } |> fail
        | (_, _) -> { ErrorResponse.Message = Some "Must be two two dimensional matricies for this function." } |> fail

    let inline slowMatrixMultiplication m1 m2 =
        match (m1,m2) with
        | (RowVector rv, TwoDimMatrix m) -> multiplyRowVectorByMatrix (RowVector rv) (TwoDimMatrix m)
        | (TwoDimMatrix m, RowVector rv) -> multiplyRowVectorByMatrix (RowVector rv) (TwoDimMatrix m)
        | (ColumnVector cv, TwoDimMatrix m) -> multiplyColumnVectorByMatrix (ColumnVector cv) (TwoDimMatrix m)
        | (TwoDimMatrix m, ColumnVector cv) -> multiplyColumnVectorByMatrix (ColumnVector cv) (TwoDimMatrix m)
        | (TwoDimMatrix m1, TwoDimMatrix m2) -> slowTwoDimMatrixMultiplication (TwoDimMatrix m1) (TwoDimMatrix m2)
        | (_, _) -> Error { ErrorResponse.Message = Some "Not a supported multiplication." }

    let (<*>) = slowMatrixMultiplication


