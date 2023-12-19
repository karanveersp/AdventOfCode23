open Solutions

open System.IO

let input = File.ReadAllText("inputs/day5.txt")


open System.Diagnostics

let measureFunctionRuntime myFunction input =
    let stopwatch = Stopwatch.StartNew()
    let result = myFunction input
    stopwatch.Stop()
    let elapsed = stopwatch.Elapsed
    (result, elapsed)

let result, elapsed = measureFunctionRuntime day5.lowestLocationNumberForSeedRangesAsync input
printfn "%d" result
printfn "runtime: %A" elapsed