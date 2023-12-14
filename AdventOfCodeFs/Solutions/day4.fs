module Solutions.day4

open System
open System.Text.RegularExpressions
open Util

type ScratchCard =
    { Id: int
      WinningNumbers: int Set
      Numbers: int list }

    static fromString (s: string) : ScratchCard =
        let pattern = @"Card\s+(\d+):\s([\d\s]+)\s\|\s([\d\s]+)"
        let matches = Regex.Match(s, pattern)

        let id, winningNumStr, numsStr =
            matches.Groups[1].Value, matches.Groups[2].Value, matches.Groups[3].Value

        let winningNumbers =
            winningNumStr.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
            |> Set.ofArray

        let numbers =
            numsStr.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
            |> Array.toList

        { Id = (id |> int)
          WinningNumbers = winningNumbers
          Numbers = numbers }


let calculatePoints (input: string) : int =
    let score (card: ScratchCard) : int =
        card.Numbers
        |> List.fold
            (fun points n ->
                match points, card.WinningNumbers.Contains(n) with
                | 0, true -> 1
                | _, true -> points * 2
                | _ -> points)
            0

    input
    |> toLines
    |> List.map ScratchCard.fromString
    |> List.map score
    |> List.sum

let calculateScratchCards (input: string) : int =
    let numMatches (card: ScratchCard) : int =
        card.Numbers
        |> List.filter (fun n -> card.WinningNumbers.Contains(n))
        |> List.length

    let originalCards = input |> toLines |> List.map ScratchCard.fromString

    let produceCopies (n: int) (card: ScratchCard) : ScratchCard list =
        match n, card.Id with
        | 0, _ -> [] // no copies
        | n, id -> originalCards[id .. (id + n - 1)]

    let rec processCards (cardsList: ScratchCard list) (count: int) =
        match cardsList with
        | [] -> count
        | head :: tail ->
            let n = numMatches head
            let wonCards = produceCopies n head
            processCards (List.append wonCards tail) (1 + count)

    processCards originalCards 0
