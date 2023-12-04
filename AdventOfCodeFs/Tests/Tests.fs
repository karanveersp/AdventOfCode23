module Tests

open System.IO
open Solutions
open Xunit

[<Fact>]
let ``Day 1 - part 1`` () =
    let input = File.ReadAllText("inputs/day1.txt")
    let output = day1.sumCalibrationValues input
    Assert.Equal(54573, output)

[<Fact>]
let ``Day 1 - part 2`` () =
    let input = File.ReadAllText("inputs/day1.txt")
    let output = day1.sumCalibrationValuesDigitWords input
    Assert.Equal(54591, output)

[<Fact>]
let ``Day 2 - sample part 1`` () =
    let input = File.ReadAllText("inputs/day2sample.txt")
    let output = day2.sumPossibleGameIds input 12 13 14
    Assert.Equal(8, output)

[<Fact>]
let ``Day 2 - part 1`` () =
    let input = File.ReadAllText("inputs/day2.txt")
    let output = day2.sumPossibleGameIds input 12 13 14
    Assert.Equal(2149, output)

[<Fact>]
let ``Day 2 - sample part 2`` () =
    let input = File.ReadAllText("inputs/day2sample.txt")
    let output = day2.sumPowersOfMinCubeQuantities input
    Assert.Equal(2286, output)

[<Fact>]
let ``Day 2 - part 2`` () =
    let input = File.ReadAllText("inputs/day2.txt")
    let output = day2.sumPowersOfMinCubeQuantities input
    Assert.Equal(71274, output)
