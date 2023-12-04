open System.IO
open Solutions

let input = File.ReadAllText("inputs/day2.txt")

printfn "%d" (day2.sumPowersOfMinCubeQuantities input)

