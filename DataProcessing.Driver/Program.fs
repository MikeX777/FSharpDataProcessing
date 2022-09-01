open DataProcessing.Railway
open DataProcessing.Matrix


let testMatrix1 = [[1;2;3;]
                   [4;5;6;]
                   [7;8;9]]
                   
                   
let testMatrix2 = [[1;2;]
                   [4;5;]
                   [7;8;]
                   [9;10;]
                   [11;12;]]
                   
                   
let testMatrix3 = [[1;2;3;4;]
                   [5;6;7;8;]]

let weakMatrix1 = createTwoDimMatrix testMatrix1
let weakMatrix2 = createTwoDimMatrix testMatrix2
let weakMatrix3 = createTwoDimMatrix testMatrix3

let weakTranspose1 = match weakMatrix1 with
                    | Success m -> transpose m |> succeed
                    | Error e -> Error e
                    
let weakTranspose2 = match weakMatrix2 with
                    | Success m -> transpose m |> succeed
                    | Error e -> Error e

let weakTranspose3 = match weakMatrix3 with
                    | Success m -> transpose m |> succeed
                    | Error e -> Error e


printfn "%A" weakTranspose1
printfn "%A" weakTranspose2
printfn "%A" weakTranspose3