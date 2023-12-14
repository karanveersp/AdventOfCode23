module Solutions.Util

open System

let toLines (input: string) : string list =
    input.Split("\n", StringSplitOptions.TrimEntries) |> Array.toList


