module Solutions.day1

open System
open System.Runtime.CompilerServices
open Util
open FSharp.Core

[<Extension>]
type StringExtensions =
    [<Extension>]
    static member inline Reverse(this: string) =
        let rev = this.ToCharArray() |> Array.rev
        String.Join("", rev)

/// Adds head/tail pattern matching on strings without having to
/// convert to list.
let (|HeadTail|Empty|) (s: string) =
    if s |> String.IsNullOrEmpty then
        Empty
    else
        HeadTail(s[0], s[1..])

let sumCalibrationValues (input: string) : int =
    let rec getFirstDigit (s: string) : string =
        match s with
        | Empty -> failwith "no digits"
        | HeadTail(head, tail) ->
            if head |> Char.IsDigit then
                head.ToString()
            else
                getFirstDigit tail

    input
    |> stringToLines
    |> List.map (fun line ->
        let fst = getFirstDigit line
        let snd = getFirstDigit (line.Reverse())
        Int32.Parse(fst + snd))
    |> List.sum

let sumCalibrationValuesDigitWords (input: string) : int =
    let digitsMap =
        [ ("one", 1)
          ("two", 2)
          ("three", 3)
          ("four", 4)
          ("five", 5)
          ("six", 6)
          ("seven", 7)
          ("eight", 8)
          ("nine", 9) ]
        |> Map.ofList

    let findFirstDigitWord (line: string) : string =
        let rec helper (line: string) (index: int) : string =
            if index = line.Length then
                failwith "no digit!"
            elif line[index] |> Char.IsDigit then
                line[index].ToString()
            else
                let substr = line[0 .. index + 1]
                let digit = digitsMap.Keys |> Seq.tryFind substr.Contains

                match digit with
                | Some d -> digitsMap[d].ToString()
                | None -> helper line (index + 1) // recurse

        helper line 0

    let findLastDigitWord (line: string) : string =
        let rec helper (line: string) (index: int) : string =
            if index = -1 then
                failwith "no digit!"
            elif line[index] |> Char.IsDigit then
                line[index].ToString()
            else
                let substr = line[index .. line.Length]
                let digit = digitsMap.Keys |> Seq.tryFind substr.Contains

                match digit with
                | Some d -> digitsMap[d].ToString()
                | None -> helper line (index - 1) // recurse

        helper line (line.Length - 1)

    input
    |> stringToLines
    |> List.map (fun line ->
        let fst = findFirstDigitWord line
        let snd = findLastDigitWord line
        Int32.Parse(fst + snd))
    |> List.sum
