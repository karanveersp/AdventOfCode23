open System.IO
open Solutions

let input = File.ReadAllText("inputs/day3.txt")

printfn "%d" (day3.sumPartNumbers input)

