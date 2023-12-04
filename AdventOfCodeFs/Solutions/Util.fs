module Solutions.Util

open System

let stringToLines (input: string) : string list =
    input.Split("\n", StringSplitOptions.TrimEntries) |> Array.toList


