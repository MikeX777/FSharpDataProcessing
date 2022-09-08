namespace WeakMatrixTests

module WeakMatrixColumnVectorMultiplicationTests =
    
    open System
    open Xunit
    open DataProcessing.Result
    open DataProcessing.WeakMatrix

    [<Fact>]
    let ``TwoDimMatrix multiplication with Column Vector Succeeds with Proper Dimensions`` () =
        // Set up
        let twoDim =            [[1;2;3;];
                                [4;5;6;];
                                [7;8;9;];]

        let columnVector =      [8;10;12;]
        let expectedVector =    [64;154;244]

        let twoDimMatrix = createTwoDimMatrix twoDim
        let columnMatrix = createColumnMatix columnVector
        let expectedMatrix = createColumnMatix expectedVector

        // Act
        match (twoDimMatrix,columnMatrix) with
        | (Success m, Success v) ->
            let outcome = multiplyColumnVectorByMatrix v m
            // Assert
            Assert.Equal(expectedMatrix, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed test.")


    [<Fact>]
    let ``Wide TwoDimMatrix multiplication with Column Vector Succeeds with Proper Dimensions`` () =
        // Set Up
        let twoDim =            [[1;2;8;7;6;];
                                [5;6;3;4;4;];]

        let columnVector =      [6;8;2;4;1;]
        let expectedVector =    [72;104;]

        let twoDimMatrix = createTwoDimMatrix twoDim
        let columnMatrix = createColumnMatix columnVector
        let expectedMatrix = createColumnMatix expectedVector

        // Act
        match (twoDimMatrix,columnMatrix) with
        | (Success m, Success v) ->
            let outcome = multiplyColumnVectorByMatrix v m
            // Assert
            Assert.Equal(expectedMatrix, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed test.")

    [<Fact>]
    let ``Tall TwoDimMatrix multiplication with Column Vector Succeeds with Proper Dimensions`` () = 
        // Set Up
        let twoDim =            [[1;5;];
                                [3;6;];
                                [8;4;];
                                [2;1;];
                                [3;6;];]

        let columnVector =      [6;2;]
        let expectedVector =    [16;30;56;14;30]
        
        let twoDimMatrix = createTwoDimMatrix twoDim
        let columnMatrix = createColumnMatix columnVector
        let expectedMatrix = createColumnMatix expectedVector

        // Act
        match (twoDimMatrix,columnMatrix) with
        | (Success m, Success v) ->
            let outcome = multiplyColumnVectorByMatrix v m
            // Assert
            Assert.Equal(expectedMatrix, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed test.")


    [<Fact>]
    let ``Vector and Matrix with improper dimensions cannot be multiplied`` () =
        // Set UP
        let twoDim =            [[1;3;];
                                [2;8;];
                                [8;5;];
                                [2;3;];
                                [1;8;];]

        let columnVector =      [1;6;8;2;4;]
        let expectedError =     Error { ErrorResponse.Message = Some "Vector and matrix cannot be multiplied." }

        let twoDimMatrix = createTwoDimMatrix twoDim
        let columnMatrix = createColumnMatix columnVector

        // Act
        match (twoDimMatrix,columnMatrix) with
        | (Success m, Success v) ->
            let outcome = multiplyColumnVectorByMatrix v m
            // Assert
            Assert.Equal(expectedError, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed test.")

    [<Fact>]
    let ``Calling Column Multiplication with a Row Vector Fails with Error`` () = 
        // Set Up
        let twoDim =            [[1;5;];
                                [3;6;];
                                [8;4;];
                                [2;1;];
                                [3;6;];]

        let rowVector =         [6;2;]
        
        let twoDimMatrix = createTwoDimMatrix twoDim
        let rowMatrix = createRowMatix rowVector
        let expectedError = Error { ErrorResponse.Message = Some "Not a supported vector and matrix for this function." }

        // Act
        match (twoDimMatrix,rowMatrix) with
        | (Success m, Success v) ->
            let outcome = multiplyColumnVectorByMatrix v m
            // Assert
            Assert.Equal(expectedError, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed test.")

    [<Fact>]
    let ``Calling Column Multiplication with a Two TwoDim Matricies Fails with Error`` () = 
        // Set Up
        let twoDim =            [[1;5;];
                                [3;6;];
                                [8;4;];
                                [2;1;];
                                [3;6;];]

        let twoDim2 =           [[6;2;];
                                [3;6;];]
        
        let twoDimMatrix1 = createTwoDimMatrix twoDim
        let twoDimMatrix2 = createTwoDimMatrix twoDim2
        let expectedError = Error { ErrorResponse.Message = Some "Not a supported vector and matrix for this function." }

        // Act
        match (twoDimMatrix1,twoDimMatrix2) with
        | (Success m1, Success m2) ->
            let outcome = multiplyColumnVectorByMatrix m1 m2
            // Assert
            Assert.Equal(expectedError, outcome)
        | (_, _) -> Assert.True(false, "Error creating weak matrix. Failed test.")

