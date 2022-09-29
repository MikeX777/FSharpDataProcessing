namespace WeakMatrixTests

open Xunit
open DataProcessing.Result
open DataProcessing.WeakMatrix

module WeakMatrixMultiplicationTests =

    [<Fact>]
    let ``TwoDimMatrix Mulitiplication Succeeds with Proper Dimensions`` () =
        // Set up
        let twoDim1 =           [[1;2;];
                                [0;1;];
                                [2;3;];]

        let twoDim2 =           [[2;5;1;1;1;];
                                [6;7;1;1;1;];]

        let expectedTwoDim =    [[14;19;3;3;3;];
                                [6;7;1;1;1;];
                                [22;31;5;5;5;];]

        let twoDimMatrix1 = createTwoDimMatrix twoDim1
        let twoDimMatrix2 = createTwoDimMatrix twoDim2
        let expectedMatrix = createTwoDimMatrix expectedTwoDim

        // Act
        match (twoDimMatrix1,twoDimMatrix2) with
        | (Success m1, Success m2) ->
            let outcome = slowTwoDimMatrixMultiplication m1 m2

            // Assert
            Assert.Equal(expectedMatrix, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Non Square TwoDimMatricies with Same Dimensions Cannot Multiply`` () =
        // Set up
        let twoDim1 =           [[1;2;];
                                [0;1;];
                                [2;3;];
                                [3;6;];
                                [2;6;];]

        let twoDim2 =           [[2;5;];
                                [3;5;];
                                [8;9;];
                                [3;4;];
                                [2;8;];]

        let expectedError = Error { ErrorResponse.Message = Some "The dimensions of the matricies do not match for multiplication." }

        let twoDimMatrix1 = createTwoDimMatrix twoDim1
        let twoDimMatrix2 = createTwoDimMatrix twoDim2

        // Act
        match (twoDimMatrix1,twoDimMatrix2) with
        | (Success m1, Success m2) ->
            let outcome = slowTwoDimMatrixMultiplication m1 m2

            // Assert
            Assert.Equal(expectedError, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Square TwoDimMatricies with Same Dimensions Can Multiply`` () =
        // Set up
        let twoDim1 =           [[1;2;6;];
                                [0;1;3;];
                                [2;3;2;];]

        let twoDim2 =           [[2;5;1;];
                                [6;7;1;];
                                [2;5;8;];]

        let expectedTwoDim =    [[26;49;51;];
                                [12;22;25;];
                                [26;41;21;];]

        let twoDimMatrix1 = createTwoDimMatrix twoDim1
        let twoDimMatrix2 = createTwoDimMatrix twoDim2
        let expectedMatrix = createTwoDimMatrix expectedTwoDim

        // Act
        match (twoDimMatrix1,twoDimMatrix2) with
        | (Success m1, Success m2) ->
            let outcome = slowTwoDimMatrixMultiplication m1 m2

            // Assert
            Assert.Equal(expectedMatrix, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Column Matrix cannot be first matrix in TwoDimMatrix multiplication`` () =
        // Set up
        let twoDim =    [[1;2;6;];
                        [0;1;3;];
                        [2;3;2;];]
        
        let column =    [3;9;8;]

        let expected =  Error { ErrorResponse.Message = Some "Must be two two dimensional matricies for this function." }

        let twoDimMatrix = createTwoDimMatrix twoDim
        let columnMatrix = createColumnMatrix column

        // Act
        match (twoDimMatrix,columnMatrix) with
        | (Success m, Success c) ->
            let outcome = slowTwoDimMatrixMultiplication c m

            // Assert
            Assert.Equal(expected, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Column Matrix cannot be second matrix in TwoDimMatrix multiplication`` () =
        // Set up
        let twoDim =    [[1;2;6;];
                        [0;1;3;];
                        [2;3;2;];]
        
        let column =    [3;9;8;]

        let expected =  Error { ErrorResponse.Message = Some "Must be two two dimensional matricies for this function." }

        let twoDimMatrix = createTwoDimMatrix twoDim
        let columnMatrix = createColumnMatrix column

        // Act
        match (twoDimMatrix,columnMatrix) with
        | (Success m, Success c) ->
            let outcome = slowTwoDimMatrixMultiplication m c

            // Assert
            Assert.Equal(expected, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Row Matrix cannot be first matrix in TwoDimMatrix multiplication`` () =
        // Set up
        let twoDim =    [[1;2;6;];
                        [0;1;3;];
                        [2;3;2;];]
        
        let row =       [3;9;8;]

        let expected =  Error { ErrorResponse.Message = Some "Must be two two dimensional matricies for this function." }

        let twoDimMatrix = createTwoDimMatrix twoDim
        let rowMatrix = createRowMatrix row

        // Act
        match (twoDimMatrix,rowMatrix) with
        | (Success m, Success r) ->
            let outcome = slowTwoDimMatrixMultiplication r m

            // Assert
            Assert.Equal(expected, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Row Matrix cannot be second matrix in TwoDimMatrix multiplication`` () =
        // Set up
        let twoDim =    [[1;2;6;];
                        [0;1;3;];
                        [2;3;2;];]
        
        let row =    [3;9;8;]

        let expected =  Error { ErrorResponse.Message = Some "Must be two two dimensional matricies for this function." }

        let twoDimMatrix = createTwoDimMatrix twoDim
        let rowMatrix = createRowMatrix row

        // Act
        match (twoDimMatrix,rowMatrix) with
        | (Success m, Success r) ->
            let outcome = slowTwoDimMatrixMultiplication m r

            // Assert
            Assert.Equal(expected, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

module WeakMatrixInfixMultiplicationTests =

    [<Fact>]
    let ``TwoDimMatrix Infix Mulitiplication Succeeds with Proper Dimensions`` () =
        // Set up
        let twoDim1 =           [[1;2;];
                                [0;1;];
                                [2;3;];]

        let twoDim2 =           [[2;5;1;1;1;];
                                [6;7;1;1;1;];]

        let expectedTwoDim =    [[14;19;3;3;3;];
                                [6;7;1;1;1;];
                                [22;31;5;5;5;];]

        let twoDimMatrix1 = createTwoDimMatrix twoDim1
        let twoDimMatrix2 = createTwoDimMatrix twoDim2
        let expectedMatrix = createTwoDimMatrix expectedTwoDim

        // Act
        match (twoDimMatrix1,twoDimMatrix2) with
        | (Success m1, Success m2) ->
            let outcome = m1 <*> m2

            // Assert
            Assert.Equal(expectedMatrix, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Non Square TwoDimMatricies with Same Dimensions Cannot Infix Multiply`` () =
        // Set up
        let twoDim1 =           [[1;2;];
                                [0;1;];
                                [2;3;];
                                [3;6;];
                                [2;6;];]

        let twoDim2 =           [[2;5;];
                                [3;5;];
                                [8;9;];
                                [3;4;];
                                [2;8;];]

        let expectedError = Error { ErrorResponse.Message = Some "The dimensions of the matricies do not match for multiplication." }

        let twoDimMatrix1 = createTwoDimMatrix twoDim1
        let twoDimMatrix2 = createTwoDimMatrix twoDim2

        // Act
        match (twoDimMatrix1,twoDimMatrix2) with
        | (Success m1, Success m2) ->
            let outcome = m1 <*> m2

            // Assert
            Assert.Equal(expectedError, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")

    [<Fact>]
    let ``Square TwoDimMatricies with Same Dimensions Can Infix Multiply`` () =
        // Set up
        let twoDim1 =           [[1;2;6;];
                                [0;1;3;];
                                [2;3;2;];]

        let twoDim2 =           [[2;5;1;];
                                [6;7;1;];
                                [2;5;8;];]

        let expectedTwoDim =    [[26;49;51;];
                                [12;22;25;];
                                [26;41;21;];]

        let twoDimMatrix1 = createTwoDimMatrix twoDim1
        let twoDimMatrix2 = createTwoDimMatrix twoDim2
        let expectedMatrix = createTwoDimMatrix expectedTwoDim

        // Act
        match (twoDimMatrix1,twoDimMatrix2) with
        | (Success m1, Success m2) ->
            let outcome = m1 <*> m2

            // Assert
            Assert.Equal(expectedMatrix, outcome)
        | (_,_) -> Assert.True(false, "Error creating weak matrix. Failed Test")
