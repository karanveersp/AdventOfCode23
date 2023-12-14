open Solutions

open System.IO

let input = File.ReadAllText("inputs/day4sample.txt")

printfn "%d" (day4.calculateScratchCards input)

