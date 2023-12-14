module Solutions.day3
open System
open Microsoft.FSharp.Core

type Location = int * int

let private isDigit (s: string): bool =
    s |> Char.Parse |> Char.IsDigit
let private isSymbol (s: string): bool =
    s <> "." && s |> isDigit |> not

let private isEmpty (s: string): bool =
    s = "."

type PartNumber = {
    value: int
    gearLocation: Location option
    neighbours: Location list
}

// keeps location in grid bounds
let private tryLocation (grid: string list list) (loc: int*int): Location option =
    let i, j = loc
    match i, j with
    | i, _ when i < 0 || i >= grid.Length -> None
    | _, j when j < 0 || j >= grid[i].Length -> None
    | _ -> Some (i, j)

type Position =
    | Top
    | Bottom
    | Left
    | Right
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight

let private top i j: Location = i-1, j
let private bottom i j: Location = i+1, j
let private left i j: Location = i, j-1
let private right i j: Location = i, j+1
let private topLeft i j: Location = i-1, j-1
let private topRight i j: Location = i-1, j+1
let private botLeft i j: Location = i+1, j-1
let private botRight i j: Location = i+1, j+1

let private tryGetValue (grid: 'a list list) (loc: Location) (neighbours: Location list) (position: Position): 'a option =
    let i, j = loc
    let tryFindNeighbour (neighbourLoc: Location) =
        neighbours
        |> List.tryFind (fun loc -> loc = neighbourLoc)
        |> Option.map (fun loc -> grid[fst loc][snd loc])
   
    match position with
    | Top ->
        top i j |> tryFindNeighbour
    | Bottom ->
        bottom i j |> tryFindNeighbour
    | Left ->
        left i j |> tryFindNeighbour
    | Right ->
        right i j |> tryFindNeighbour
    | TopLeft ->
        topLeft i j |> tryFindNeighbour
    | TopRight ->
        topRight i j |> tryFindNeighbour
    | BottomLeft ->
        botLeft i j |> tryFindNeighbour
    | BottomRight ->
        botRight i j |> tryFindNeighbour

type Cell = {
    IsNumberPart: bool
    IsNumberStart: bool
    IsNumberEnd: bool
    IsSymbol: bool
    IsEmpty: bool
    Value: string option
    Location: Location option
    Neighbors: Location list
} with
    static member Empty =
        {
            Value = None
            IsNumberPart = false
            IsNumberStart = false
            IsNumberEnd = false
            IsSymbol = false
            IsEmpty = false
            Location = None
            Neighbors = [] 
        }
    static member getNeighbourLocations (grid: string list list) (location: int*int): Location list =
        let i, j = location
        [left i j; right i j; top i j; bottom i j; topLeft i j; topRight i j; botLeft i j; botRight i j]
        |> List.choose (tryLocation grid)
   
    static member from (grid: string list list) (i: int) (j: int) (v: string): Cell =
        let neighbours = Cell.getNeighbourLocations grid (i, j)
        let getValueAt = tryGetValue grid (i,j) neighbours
        match v with
        | "." ->
            { Cell.Empty with Value = Some "."; Location = Some (i, j); IsEmpty = true; Neighbors = neighbours }
        | digit when digit |> isDigit ->
            // determine if start, part or ending digit.
            
            let leftVal, rightVal = getValueAt Left, getValueAt Right
            
            match leftVal, rightVal with
            | None, Some rightString ->
                if rightString |> isDigit then
                    // start of number
                    { Cell.Empty with Value = Some digit; Location = Some (i, j); IsNumberStart = true; Neighbors = neighbours }
                else
                    // start and end of number
                    { Cell.Empty with Value = Some digit; Location = Some (i, j); IsNumberStart = true; IsNumberEnd = true; Neighbors = neighbours }

            | Some leftString, Some rightString ->
                if leftString |> isDigit && rightString |> isDigit then
                    // middle of number
                    { Cell.Empty with Value = Some digit; Location = Some (i, j); IsNumberPart = true; Neighbors = neighbours }
                elif leftString |> isDigit then
                    // end of number
                    { Cell.Empty with Value = Some digit; Location = Some (i, j); IsNumberEnd = true; Neighbors = neighbours }
                elif rightString |> isDigit then
                    // start of number
                    { Cell.Empty with Value = Some digit; Location = Some (i, j); IsNumberStart = true; Neighbors = neighbours }
                else
                    // single digit number
                    { Cell.Empty with Value = Some digit; Location = Some (i, j); IsNumberStart = true; IsNumberEnd = true; Neighbors = neighbours }
            | Some leftString, None ->
                if leftString |> isDigit then
                    // end of number
                    { Cell.Empty with Value = Some digit; Location = Some (i, j); IsNumberEnd = true; Neighbors = neighbours }
                else
                    // start and end of number
                    { Cell.Empty with Value = Some digit; Location = Some (i, j); IsNumberStart = true; IsNumberEnd = true; Neighbors = neighbours }
            | None, None -> failwith "No left or right neighbours!"
            
        | symbol ->
            // is symbol
            { Cell.Empty with Value = Some symbol; Location = Some (i, j); IsSymbol = true; Neighbors = neighbours }

let private isSymbolInNeighbours (grid: Cell list list) (neighbours: Location list): bool =
    neighbours
    |> List.exists (fun loc ->
        let i, j = loc
        (grid[i][j]).Value.Value
        |> isSymbol)

let private tryFindGearInNeighbours (grid: Cell list list) (neighbours: Location list): Location option =
    neighbours
    |> List.tryFind (fun loc ->
        let i, j = loc
        (grid[i][j]).Value.Value = "*")
    
let rec private buildCompleteNumberAccumulatingNeighbours (cellGrid: Cell list list) (cell: Cell) (numbers: string) neighbours =
    if cell.IsNumberEnd then
        ((numbers + cell.Value.Value) |> int, cell.Neighbors |> List.append neighbours |> List.distinct)
    else
        let rightCell = tryGetValue cellGrid cell.Location.Value cell.Neighbors Position.Right
        match rightCell with
        | None ->
            failwith "reached rightmost cell while building a number, that wasn't an ending digit!"
        | Some rc ->
            buildCompleteNumberAccumulatingNeighbours cellGrid rc (numbers+cell.Value.Value) (List.append neighbours cell.Neighbors)
            
let private printGrid (cellGrid: Cell list list) =
    cellGrid
    |> List.iter (fun row ->
         row |> List.iter (fun cell ->
             printf $" {cell.Value.Value} "
             )
         printfn ""
         )

let private cellGridFromString(input: string): Cell list list =
    let grid: string list list =
        Util.toLines input
        |> List.map (fun line -> line.ToCharArray() |> Array.toList |> List.map string)
    let mutable cellGrid: Cell list list = []
    
    // grid -> cell grid
    grid
    |> List.iteri (fun i row ->
         let mutable cellRow: Cell list = []
         row |> List.iteri (fun j v ->
             // first turn 2d string list to 2d Cell list
             cellRow <- List.append cellRow [Cell.from grid i j v] 
             )
         cellGrid <- List.append cellGrid [cellRow])

    cellGrid
    
let sumPartNumbers (input: string): int =
    let cellGrid = cellGridFromString input

    let mutable partNumbers: int list = []
    
    cellGrid
    |> List.iter (fun row ->
        row |> List.iter (fun cell ->
            
            if cell.IsNumberStart && cell.IsNumberEnd then
                // single digit number
                let number, neighbours = cell.Value.Value |> int, cell.Neighbors
                if neighbours |> (isSymbolInNeighbours cellGrid) then
                    partNumbers <- List.append partNumbers [number]
            elif cell.IsNumberStart then
                let number, allNeighbours = buildCompleteNumberAccumulatingNeighbours cellGrid cell "" []
                if allNeighbours |> (isSymbolInNeighbours cellGrid) then
                    partNumbers <- List.append partNumbers [number]
            )
        )
     
    List.sum partNumbers

let sumGearRatios (input: string): int =
    let cellGrid = cellGridFromString input
    
    let mutable partNumbers: PartNumber list = []
    
    // collect all part numbers with possible gear neighbour
    cellGrid
    |> List.iter (fun row ->
        row |> List.iter (fun cell ->
            
            let numNeighbourOpt =
                if cell.IsNumberStart && cell.IsNumberEnd then
                    // single digit number
                    Some(cell.Value.Value |> int, cell.Neighbors)
                elif cell.IsNumberStart then
                    Some(buildCompleteNumberAccumulatingNeighbours cellGrid cell "" [])
                else
                    None
            
            match numNeighbourOpt with
            | Some(number, neighbours) ->
                let partNumber: PartNumber =
                    {value = number; neighbours = neighbours; gearLocation = tryFindGearInNeighbours cellGrid neighbours }
                partNumbers <- List.append partNumbers [partNumber]
            | None ->
                ()
            )
        )
    
    partNumbers
    |> List.filter (fun pn -> pn.gearLocation.IsSome)
    |> List.groupBy (fun pn -> pn.gearLocation.Value)
    |> List.filter (fun (_, nums) -> nums.Length = 2)
    |> List.map (fun (_, nums) -> nums[0].value * nums[1].value)
    |> List.sum
