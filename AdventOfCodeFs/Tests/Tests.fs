module Tests

open System.IO
open Solutions
open Xunit

[<Fact>]
let ``Day 1 - part 1`` () =
    let input = File.ReadAllText("day1.txt")
    let output = day1.sumCalibrationValues input
    Assert.Equal(54573, output)

[<Fact>]
let ``Day 1 - part 2`` () =
    let input = File.ReadAllText("day1.txt")
    let output = day1.sumCalibrationValuesDigitWords input
    Assert.Equal(54591, output)
