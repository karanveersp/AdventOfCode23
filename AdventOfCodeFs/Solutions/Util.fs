module Solutions.Util

open System

let toLines (input: string) : string list =
    input.Split("\n", StringSplitOptions.TrimEntries) |> Array.toList



let tryFindFirstSome<'a, 'b> (f: 'a -> 'b option) (list: 'a list) : 'b option =
    let rec loop =
        function
        | [] -> None
        | x :: xs ->
            match f x with
            | Some result -> Some result
            | None -> loop xs

    loop list