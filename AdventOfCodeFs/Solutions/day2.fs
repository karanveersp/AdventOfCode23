module Solutions.day2

open System.Text.RegularExpressions
open Util
open System

type Cube =
    | Red
    | Green
    | Blue

    static member fromString(s: string) : Cube =
        match s with
        | "red" -> Red
        | "green" -> Green
        | "blue" -> Blue
        | s -> failwith $"unrecognized color: {s}"

type Cubes = int * Cube

type Game =
    { Id: int
      Sets: Cubes list list }

    static member fromString(s: string) : Game =
        let parseGameID (s: string) : int =
            let idMatch = Regex.Match(s, @"Game (\d+)")
            idMatch.Groups[1].Value |> Int32.Parse

        let parseSets (s: string) : Cubes list list =
            s.Split(";", StringSplitOptions.TrimEntries)
            |> Array.toList
            |> List.map (fun setString ->
                setString.Split(",", StringSplitOptions.TrimEntries)
                |> Array.toList
                |> List.map (fun cubeStr ->
                    // Eg. "2 red" -> (2 * Red)
                    let cubeMatch = Regex.Match(cubeStr, @"(\d+)\s(red|green|blue)")
                    (cubeMatch.Groups[1].Value |> Int32.Parse, cubeMatch.Groups[2].Value |> Cube.fromString)))

        { Id = parseGameID s
          Sets = parseSets s }

let sumPossibleGameIds (input: string) (maxRed: int) (maxGreen: int) (maxBlue: int) : int =
    let isPossibleQty (cubes: Cubes) : bool =
        match cubes with
        | qty, Red -> qty <= maxRed
        | qty, Green -> qty <= maxGreen
        | qty, Blue -> qty <= maxBlue

    let isPossibleSet (set: Cubes list) : bool = set |> List.forall isPossibleQty

    let isPossibleGame (g: Game) : bool = g.Sets |> List.forall isPossibleSet

    toLines input
    |> List.map Game.fromString
    |> List.filter isPossibleGame
    |> List.sumBy (fun g -> g.Id)


let sumPowersOfMinCubeQuantities (input: string) : int =
    let minRequiredQtyInSets (sets: Cubes list list) (cube: Cube) : int =
        sets
        |> List.map (fun set ->
            let maxQty =
                set |> List.filter (fun (_, cubeColor) -> cubeColor = cube) |> List.map fst

            match maxQty with
            | [] -> 0 // no cubes of this color in set
            | head :: _ -> head)
        |> List.max

    let powerOfMinRequiredCubes (g: Game) : int =
        [ Red; Green; Blue ]
        |> List.map (minRequiredQtyInSets g.Sets)
        |> List.reduce (*)

    toLines input
    |> List.map Game.fromString
    |> List.map powerOfMinRequiredCubes
    |> List.sum
