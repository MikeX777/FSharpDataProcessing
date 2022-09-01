namespace DataProcessing

module Result =
    type ErrorResponse =
        {
        Message: string option
        }

    type Success<'a> = 
        {
        Success: 'a
        }

    type Result<'TSuccess, 'TError> =
        | Error of 'TError
        | Success of 'TSuccess

    let bind resultFunction twoTrackInput =
        match twoTrackInput with
        | Success s -> resultFunction s
        | Error e -> Error e

    // infix bind operator
    let (>>=) twoTrackInput switchFunction =
        bind switchFunction twoTrackInput

    let (>=>) resultFunc1 resultFunc2 =
        resultFunc1 >> (bind resultFunc2)

    let switch f x =
        f x |> Success

    let succeed x =
        Success x
    
    let fail x =
        Failure x  
