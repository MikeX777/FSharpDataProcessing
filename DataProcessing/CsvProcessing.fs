namespace DataProcessing

open System.IO
open Result

module CsvProcessing =

    let getLines path =
        try
            File.ReadAllLines path |> succeed
        with
            | :? System.ArgumentNullException as ex -> fail { Message = Some $"An ArgumentNullException was thrown while trying to read file. Please pass a valid path. Message: {ex.Message}" }
            | :? System.IO.PathTooLongException as ex -> fail { Message = Some $"A PathTooLongException was thrown while trying to read file. The path was too long to be read. Message: {ex.Message}" }
            | :? System.IO.DirectoryNotFoundException as ex -> fail { Message = Some $"A DirectoryNotFoundException was thrown while trying to read file. The directory was not found. Message: {ex.Message}" }
            | :? System.IO.IOException as ex -> fail { Message = Some $"An IOException was thrown while trying to read file. Message: {ex.Message}" }
            | :? System.UnauthorizedAccessException as ex -> fail { Message = Some $"An UnauthorizedAccessException was thrown while trying to read file. The file is readonly. Message: {ex.Message}" }
            | :? System.IO.FileNotFoundException as ex -> fail { Message = Some $"A FileNotFoundException was thrown while trying to read file. The file was not found in the directory. Message: {ex.Message}" }
            | :? System.NotSupportedException as ex -> fail { Message = Some $"A NotSupportedException was thrown while trying to read file. Path was in an invalid format. Message: {ex.Message}" }
            | :? System.Security.SecurityException as ex -> fail { Message = Some $"A SecurityException was thrown while trying to read file. Caller does not have access to the file. Message: {ex.Message}" }
            | :? System.Exception as ex -> fail { Message = Some $"An Exception was thrown while trying to read file. Message: {ex.Message}" }
    
    let validateNotEmpty hasHeader (lines:string[]) =
        if (not hasHeader && lines.Length > 0) || (hasHeader && lines.Length > 1) then
            lines |> succeed
        else
            { Message = Some "Error processing: Empty data file."} |> fail

    let extractHeader hasHeader (lines:string[]) =
        if hasHeader then
            (lines[0], lines[1..(lines.Length-1)]) |> succeed
        else
            ("", lines) |> succeed

    let swap x y (a: 'a []) =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    let shuffle (rand:System.Random) a =
        Array.iteri (fun i _ -> a |> swap i (rand.Next(i, Array.length a))) a

    let randomizeLines shuffleLines seed data =
        if shuffleLines then
            let rand = new System.Random(seed)
            let (headers, lines) = data
            shuffle rand lines
            (headers, lines) |> succeed
        else 
            data |> succeed

    let parseDelimter delimiterChar (data:string) =
        (data.Split [|delimiterChar|]) |> Array.toList

    let parseDataTuple delimiterChar data =
        let (headers, lines) = data 
        let rec loop lines acc =
            match lines with
            | head :: tail -> loop tail (List.append acc [(parseDelimter delimiterChar head)])
            | [] -> acc
        ((parseDelimter delimiterChar headers), (loop (lines |> Array.toList) List.empty)) |> succeed


    let parseFile shuffleLines seed hasHeader delimterChar path =
        path |>
        getLines
        >>= validateNotEmpty hasHeader 
        >>= extractHeader hasHeader
        >>= randomizeLines shuffleLines seed
        >>= parseDataTuple delimterChar

