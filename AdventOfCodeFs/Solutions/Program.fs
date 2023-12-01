open System.IO
open Solutions

let input = File.ReadAllText("day1.txt")

printfn "%d" (day1.sumCalibrationValues input)

