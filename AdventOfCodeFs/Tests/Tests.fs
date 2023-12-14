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


[<Fact>]
let ``Day 3 - sample part 1`` () =
    let input = File.ReadAllText("inputs/day3sample.txt")
    let output = day3.sumPartNumbers input
    Assert.Equal(4361, output)


[<Fact>]
let ``Day 3 - part 1`` () =
    let input = File.ReadAllText("inputs/day3.txt")
    let output = day3.sumPartNumbers input
    Assert.Equal(551094, output)


[<Fact>]
let ``Day 3 - sample part 2`` () =
    let input = File.ReadAllText("inputs/day3sample.txt")
    let output = day3.sumGearRatios input
    Assert.Equal(467835, output)

[<Fact>]
let ``Day 3 - part 2`` () =
    let input = File.ReadAllText("inputs/day3.txt")
    let output = day3.sumGearRatios input
    Assert.Equal(80179647, output)


[<Fact>]
let ``Day 4 - sample part 1`` () =
    let input = File.ReadAllText("inputs/day4sample.txt")
    let output = day4.calculatePoints input
    Assert.Equal(13, output)
 

[<Fact>]
let ``Day 4 - part 1`` () =
    let input = File.ReadAllText("inputs/day4.txt")
    let output = day4.calculatePoints input
    Assert.Equal(15268, output)


[<Fact>]
let ``Day 4 - sample part 2`` () =
    let input = File.ReadAllText("inputs/day4sample.txt")
    let output = day4.calculateScratchCards input
    Assert.Equal(30, output)

[<Fact>]
let ``Day 4 - part 2`` () =
    let input = File.ReadAllText("inputs/day4.txt")
    let output = day4.calculateScratchCards input
    Assert.Equal(6283755, output)