namespace WeakMatrixTests

module WeakMatrixTransposeTests =

    open System
    open Xunit
    open DataProcessing.Result
    open DataProcessing.WeakMatrix

    [<Fact>]
    let ``Transpose square matrix`` () =
        // Set Up
        let testMatrix =        [[1;2;3;];
                                [4;5;6;];
                                [7;8;9];]
        let expectedResult =    [[1;4;7;];
                                [2;5;8;];
                                [3;6;9;];]

        let testWeakMatrix = createTwoDimMatrix testMatrix
        let expectedWeakMatrix = createTwoDimMatrix expectedResult

        // Act
        match testWeakMatrix with
        | Error e -> Assert.True(false, "Error creating matrix, false test.")
        | Success s ->
            // Assert
            let testTranspose = transpose s
            Assert.Equal(expectedWeakMatrix, testTranspose)

    [<Fact>]
    let ``Transpose tall matrix`` () =
        // Set up
        let testMatrix =        [[1;2;];
                                [4;5;];
                                [7;8;];
                                [9;10;];
                                [11;12;];]
    
        let expectedMatrix =    [[1;4;7;9;11];
                                [2;5;8;10;12];]

        let testWeakMatrix = createTwoDimMatrix testMatrix
        let expectedWeakMatrix = createTwoDimMatrix expectedMatrix

        // Act
        match testWeakMatrix with
        | Error e -> Assert.True(false, "Error creating matrix, false test.")
        | Success s ->
            // Assert
            let testTranspose = transpose s
            Assert.Equal(expectedWeakMatrix, testTranspose)

    [<Fact>]
    let ``Transpose wide matrix`` () =
        // Set Up
        let testMatrix =        [[1;2;3;4];
                                [5;6;7;8;];]

        let expectedMatrix =    [[1;5;];
                                [2;6;];
                                [3;7;];
                                [4;8;];]

        let testWeakMatrix = createTwoDimMatrix testMatrix
        let expectedWeakMatrix = createTwoDimMatrix expectedMatrix

        // Act
        match testWeakMatrix with
        | Error e -> Assert.True(false, "Error creating matrix, false test.")
        | Success s ->
            // Assert
            let testTranspose = transpose s
            Assert.Equal(expectedWeakMatrix, testTranspose)



