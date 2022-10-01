namespace WeakMatrixTests

open Xunit
open DataProcessing.Result
open DataProcessing.WeakMatrix

module WeakMatrixAdditionTests =

    [<Fact>]
    let ``Two Column vectors of equvialent lengths can be added`` () =
        // Set up
        let column1 =   [2;5;8;7]
        let column2 =   [8;6;3;2;]
        let e =         [10;11;11;9]

        let columnMatrix1 = createColumnMatrix column1
        let columnMatrix2 = createColumnMatrix column2
        let expected = createColumnMatrix e

        // Act
        match (columnMatrix1,columnMatrix2) with
        | (Success c1, Success c2) ->
            let outcome = matrixAddition c1 c2

            // Assert
            Assert.Equal(expected, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Two Column vectors of different lengths cannot be added`` () =
        // Set up
        let column1 =   [1;5;8;9;3;]
        let column2 =   [2;0;4;]

        let expected = { ErrorResponse.Message = Some "Error, vectors did not have the same number of components." } |> fail

        let columnMatrix1 = createColumnMatrix column1
        let columnMatrix2 = createColumnMatrix column2

        // Act
        match (columnMatrix1,columnMatrix2) with
        | (Success c1, Success c2) ->
            let outcome = matrixAddition c1 c2

            // Assert
            Assert.Equal(expected, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Two Column vectors of equvialent lengths can be added with Infix`` () =
        // Set up
        let column1 =   [2;5;8;7]
        let column2 =   [8;6;3;2;]
        let e =         [10;11;11;9]

        let columnMatrix1 = createColumnMatrix column1
        let columnMatrix2 = createColumnMatrix column2
        let expected = createColumnMatrix e

        // Act
        match (columnMatrix1,columnMatrix2) with
        | (Success c1, Success c2) ->
            let outcome = c1 <+> c2

            // Assert
            Assert.Equal(expected, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed Test")


    [<Fact>]
    let ``Two row vectors of equvialent lengths can be added`` () =
        // Set up
        let row1 =      [5;1;3;2]
        let row2 =      [5;4;0;1;]
        let e =         [10;5;3;3]

        let rowMatrix1 = createRowMatrix row1
        let rowMatrix2 = createRowMatrix row2
        let expected = createRowMatrix e

        // Act
        match (rowMatrix1,rowMatrix2) with
        | (Success r1, Success r2) ->
            let outcome = matrixAddition r1 r2

            // Assert
            Assert.Equal(expected, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Two Row vectors of different lengths cannot be added`` () =
        // Set up
        let row1 =   [1;5;8;9;3;]
        let row2 =   [2;0;4;]

        let expected = { ErrorResponse.Message = Some "Error, vectors did not have the same number of components." } |> fail

        let rowMatrix1 = createRowMatrix row1
        let rowMatrix2 = createRowMatrix row2

        // Act
        match (rowMatrix1,rowMatrix2) with
        | (Success r1, Success r2) ->
            let outcome = matrixAddition r1 r2

            // Assert
            Assert.Equal(expected, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Two Row vectors of equvialent lengths can be added with Infix`` () =
        // Set up
        let row1 =      [3;4;2;6]
        let row2 =      [2;2;9;1;]
        let e =         [5;6;11;7]

        let rowMatrix1 = createRowMatrix row1
        let rowMatrix2 = createRowMatrix row2
        let expected = createRowMatrix e

        // Act
        match (rowMatrix1,rowMatrix2) with
        | (Success r1, Success r2) ->
            let outcome = r1 <+> r2

            // Assert
            Assert.Equal(expected, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed Test")


    [<Fact>]
    let ``Two two dimensional matricies of equvialent dimensions can be added`` () =
        // Set up
        let twoDim1 =   [[5;1;3;2];
                        [6;2;3;8;];
                        [9;5;3;6;];]

        let twoDim2 =   [[5;4;0;1;];
                        [2;3;8;6;];
                        [3;9;8;6;];]

        let e =         [[10;5;3;3;];
                        [8;5;11;14;];
                        [12;14;11;12;];]

        let matrix1 = createTwoDimMatrix twoDim1
        let matrix2 = createTwoDimMatrix twoDim2
        let expected = createTwoDimMatrix e

        // Act
        match (matrix1,matrix2) with
        | (Success m1, Success m2) ->
            let outcome = matrixAddition m1 m2

            // Assert
            Assert.Equal(expected, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Two two dimensional matricies of different dimensions cannot be added`` () =
        // Set up
        let twoDim1 =   [[1;5;8;9;3;];
                        [2;3;6;8;6;];
                        [2;3;6;9;7;];]

        let twoDim2 =   [[2;0;4;];
                        [3;6;9;];
                        [2;8;3;];]

        let expected =  { ErrorResponse.Message = Some "Error, two dimensional matricies do not match each other." } |> fail

        let matrix1 = createTwoDimMatrix twoDim1
        let matrix2 = createTwoDimMatrix twoDim2

        // Act
        match (matrix1,matrix2) with
        | (Success m1, Success m2) ->
            let outcome = matrixAddition m1 m2

            // Assert
            Assert.Equal(expected, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Two two dimensional matricies of equvialent dimensions can be added with Infix`` () =
        // Set up
        let twoDim1 =   [[3;4;2;6];
                        [3;6;9;2;];
                        [2;5;7;3;];
                        [3;2;1;5;];]

        let twoDim2 =   [[2;2;9;1;];
                        [3;5;8;7;];
                        [1;2;7;9;];
                        [2;6;3;0;];]

        let e =         [[5;6;11;7];
                        [6;11;17;9;];
                        [3;7;14;12;];
                        [5;8;4;5;];]

        let matrix1 = createTwoDimMatrix twoDim1
        let matrix2 = createTwoDimMatrix twoDim2
        let expected = createTwoDimMatrix e

        // Act
        match (matrix1,matrix2) with
        | (Success m1, Success m2) ->
            let outcome = m1 <+> m2

            // Assert
            Assert.Equal(expected, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed Test")

module WeakMatrixSubtractionTests =
    
    [<Fact>]
    let ``Two Column vectors of equvialent lengths can be subtracted`` () =
        // Set up
        let column1 =   [2;5;8;7]
        let column2 =   [8;6;3;2;]
        let e =         [-6;-1;5;5]

        let columnMatrix1 = createColumnMatrix column1
        let columnMatrix2 = createColumnMatrix column2
        let expected = createColumnMatrix e

        // Act
        match (columnMatrix1,columnMatrix2) with
        | (Success c1, Success c2) ->
            let outcome = matrixSubtraction c1 c2

            // Assert
            Assert.Equal(expected, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Two Column vectors of different lengths cannot be subtracted`` () =
        // Set up
        let column1 =   [1;5;8;9;3;]
        let column2 =   [2;0;4;]

        let expected = { ErrorResponse.Message = Some "Error, vectors did not have the same number of components." } |> fail

        let columnMatrix1 = createColumnMatrix column1
        let columnMatrix2 = createColumnMatrix column2

        // Act
        match (columnMatrix1,columnMatrix2) with
        | (Success c1, Success c2) ->
            let outcome = matrixSubtraction c1 c2

            // Assert
            Assert.Equal(expected, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Two Column vectors of equvialent lengths can be subtracted with Infix`` () =
        // Set up
        let column1 =   [2;5;8;7]
        let column2 =   [8;6;3;2;]
        let e =         [-6;-1;5;5]

        let columnMatrix1 = createColumnMatrix column1
        let columnMatrix2 = createColumnMatrix column2
        let expected = createColumnMatrix e

        // Act
        match (columnMatrix1,columnMatrix2) with
        | (Success c1, Success c2) ->
            let outcome = c1 <-> c2

            // Assert
            Assert.Equal(expected, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed Test")


    [<Fact>]
    let ``Two row vectors of equvialent lengths can be subtracted`` () =
        // Set up
        let row1 =      [5;1;3;2]
        let row2 =      [5;4;0;1;]
        let e =         [0;-3;3;1]

        let rowMatrix1 = createRowMatrix row1
        let rowMatrix2 = createRowMatrix row2
        let expected = createRowMatrix e

        // Act
        match (rowMatrix1,rowMatrix2) with
        | (Success r1, Success r2) ->
            let outcome = matrixSubtraction r1 r2

            // Assert
            Assert.Equal(expected, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Two Row vectors of different lengths cannot be subtracted`` () =
        // Set up
        let row1 =   [1;5;8;9;3;]
        let row2 =   [2;0;4;]

        let expected = { ErrorResponse.Message = Some "Error, vectors did not have the same number of components." } |> fail

        let rowMatrix1 = createRowMatrix row1
        let rowMatrix2 = createRowMatrix row2

        // Act
        match (rowMatrix1,rowMatrix2) with
        | (Success r1, Success r2) ->
            let outcome = matrixSubtraction r1 r2

            // Assert
            Assert.Equal(expected, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Two Row vectors of equvialent lengths can be subtracted with Infix`` () =
        // Set up
        let row1 =      [3;4;2;6]
        let row2 =      [2;2;9;1;]
        let e =         [1;2;-7;5]

        let rowMatrix1 = createRowMatrix row1
        let rowMatrix2 = createRowMatrix row2
        let expected = createRowMatrix e

        // Act
        match (rowMatrix1,rowMatrix2) with
        | (Success r1, Success r2) ->
            let outcome = r1 <-> r2

            // Assert
            Assert.Equal(expected, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed Test")


    [<Fact>]
    let ``Two two dimensional matricies of equvialent dimensions can be subtracted`` () =
        // Set up
        let twoDim1 =   [[5;1;3;2];
                        [6;2;3;8;];
                        [9;5;3;6;];]

        let twoDim2 =   [[5;4;0;1;];
                        [2;3;8;6;];
                        [3;9;8;6;];]

        let e =         [[0;-3;3;1;];
                        [4;-1;-5;2;];
                        [6;-4;-5;0;];]

        let matrix1 = createTwoDimMatrix twoDim1
        let matrix2 = createTwoDimMatrix twoDim2
        let expected = createTwoDimMatrix e

        // Act
        match (matrix1,matrix2) with
        | (Success m1, Success m2) ->
            let outcome = matrixSubtraction m1 m2

            // Assert
            Assert.Equal(expected, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Two two dimensional matricies of different dimensions cannot be subracted`` () =
        // Set up
        let twoDim1 =   [[1;5;8;9;3;];
                        [2;3;6;8;6;];
                        [2;3;6;9;7;];]

        let twoDim2 =   [[2;0;4;];
                        [3;6;9;];
                        [2;8;3;];]

        let expected =  { ErrorResponse.Message = Some "Error, two dimensional matricies do not match each other." } |> fail

        let matrix1 = createTwoDimMatrix twoDim1
        let matrix2 = createTwoDimMatrix twoDim2

        // Act
        match (matrix1,matrix2) with
        | (Success m1, Success m2) ->
            let outcome = addTwoDimMatricies m1 m2

            // Assert
            Assert.Equal(expected, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Two two dimensional matricies of equvialent dimensions can be subtracted with Infix`` () =
        // Set up
        let twoDim1 =   [[3;4;2;6];
                        [3;6;9;2;];
                        [2;5;7;3;];
                        [3;2;1;5;];]

        let twoDim2 =   [[2;2;9;1;];
                        [3;5;8;7;];
                        [1;2;7;9;];
                        [2;6;3;0;];]

        let e =         [[1;2;-7;5];
                        [0;1;1;-5;];
                        [1;3;0;-6;];
                        [1;-4;-2;5;];]

        let matrix1 = createTwoDimMatrix twoDim1
        let matrix2 = createTwoDimMatrix twoDim2
        let expected = createTwoDimMatrix e

        // Act
        match (matrix1,matrix2) with
        | (Success m1, Success m2) ->
            let outcome = m1 <-> m2

            // Assert
            Assert.Equal(expected, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed Test")
