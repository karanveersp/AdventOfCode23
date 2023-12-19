module Solutions.day5

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks

let parseSpaceDelimNumbers (line: string) : int64 list =
    line.Split(" ", StringSplitOptions.TrimEntries)
    |> Array.filter (String.IsNullOrEmpty >> not)
    |> Array.map int64
    |> Array.toList

type NumRange =
    { first: int64
      last: int64 }
    
    
    member this.intersection(other: NumRange): NumRange option =
        if this.intersects(other) then
            Some { first = max this.first other.first; last = min this.last other.last } 
        else
            None
    
    member this.intersects(other: NumRange): bool =
        (this.first >= other.first && this.first <= other.last) ||
        (this.last >= other.first && this.last <= other.last) ||
        (other.first >= this.first && other.first <= this.last) ||
        (other.last >= this.first && other.last <= this.last)
    
    // Lets one split this range into numerous ranges excluding the given range.
    // Returns a range without the overlapping values of the other range.

    member this.difference(other: NumRange): NumRange list * bool =
        match this.intersection(other) with
        | Some intersection ->
            if this.first = intersection.first && this.last = intersection.last then
                // all points intersect
                [], true
            elif this.first = intersection.first then
                [ {first = intersection.last+1L; last = this.last} ], true
            elif this.last = intersection.last then
                [ {first = this.first; last = intersection.first-1L} ], true
            else
                // disjoint range
                [
                    { first = this.first; last = intersection.first-1L}
                    { first = intersection.last+1L; last = this.last }
                ], true
        | None -> [this], false 


type Rule =
    { sourceRange: NumRange
      destRange: NumRange }
    
    // /// Map key range to a value range
    // member this.mapRange(kr: NumRange) : NumRange list =
    //     let minIntersects, maxIntersects =
    //         this.keyRange.containsMinValueOf (kr), this.keyRange.containsMaxValueOf (kr)
    //
    //     match minIntersects, maxIntersects with
    //     | true, true ->
    //         // all keys are in there
    //         let minOffset = kr.first - this.keyRange.first
    //         let len = kr.last - kr.first
    //         
    //         let int{ min = this.valueRange.min + minOffset
    //           max = this.valueRange.min + len }
    //
    //     | true, false ->
    //         // min key is in there
    //         //  -------- (this.keyRange)
    //         //      *--------- (kr)
    //         // Now we compute value range for this key range.
    //         let minOffSet = kr.first - this.keyRange.first
    //
    //         { first = this.valueRange.first + minOffSet
    //           last = kr.last }
    //
    //     | false, true ->
    //         //           ----------- (this.keyRange)
    //         //      ---------* (kr)
    //         let maxOffset = kr.last - this.keyRange.first
    //         { first = kr.first; last = kr.last }
    //     | false, false ->
    //         kr // identity - pretend its in there and found


type RangeMap =
    { Rules: Rule list }

    /// for part 1, we send int64 values and map
    member this.get(key: int64) : int64 =
        let rec checkRules (rules: Rule list) =
            match rules with
            | [] -> key // value is key (identity)
            | rule :: remaining ->
                if rule.sourceRange.first <= key && rule.sourceRange.last >= key then
                    // key is present!
                    // compute dest value
                    rule.destRange.first + (key - rule.sourceRange.first)
                else
                    checkRules remaining

        checkRules this.Rules

    // /// Returns a tuple of (mapped ranges, and unmapped ranges)
    // member this.getValueRanges(keyRange: NumRange): NumRange list * NumRange list =
    //     let mutable unmappedRanges: NumRange list = []
    //     let mutable intersections: NumRange list = []
    //     
    //     this.Rules
    //     |> List.iter (fun rule ->
    //         // Different rules can have different intersects with input.
    //         // Iterate through rules, and only if there's an intersect, add the intersecting values to the mapped values.
    //         match keyRange.intersection(rule.sourceRange) with
    //         | Some intersection ->
    //             List.append intersections [intersection]
    //             
    //         | None -> ()
    //         )
    //     // all mapped values are accumulated, these become the input ranges for the next step.
    //     
    //     
    //     let mappedValues =
    //         intersections
    //         |> List.map (fun intersection ->
    //                 // map intersection to values, by determining distance from start.
    //                 let len = intersection.last - intersection.first
    //                 let first = rule.destRange.first + (intersection.first - rule.sourceRange.first)
    //                 { first = first; last = first + len }
    //             )
    //     
    //     let unmapped
    //     
    //     let rec toRanges (inputRange: NumRange) (rangesToCheck: KeyValRange list) (valueRanges: NumRange list) =
    //         match rangesToCheck with
    //         | [] -> // processed all ranges
    //             valueRanges 
    //         | keyValRange :: tail ->
    //             match inputRange.intersectionWith(keyValRange) with
    //             | Some (intersection, _) ->
    //                 // there is an intersection range with the current keyvalrange.
    //                 let unMappedRangesList = inputRange.differenceWith(intersection)
    //                 // compute value range
    //                 match keyValRange.getValueRange(intersection) with
    //                 | Some vr ->
    //                     // recurse
    //                     toRanges unMapped tail (List.append valueRanges [vr])
    //                 
    //             | None ->
    //                 // no intersection of input with range, recurse to next keyvalrange
    //                 toRanges inputRange tail valueRanges
    //                 
        
        


let parseMapFromLines (reverse: bool) (matchedLines: string): RangeMap =
    let rules =
        matchedLines.Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line ->
            let parts = parseSpaceDelimNumbers line
            let dest, source, len =
                if reverse then
                    // swap dest/source
                    parts[1], parts[0], parts[2]
                else
                    parts[0], parts[1], parts[2]

            { sourceRange =
                { first = source
                  last = source + len - 1L }
              destRange = { first = dest; last = dest + len - 1L } })
        |> Array.sortBy (fun kr -> kr.sourceRange.first)
        |> Array.toList

    { Rules = rules }


let parseSeeds (s: string) : int64 list =
    let matches = Regex.Match(s, @"seeds:([\d\s]+)")
    matches.Groups[1].Value |> parseSpaceDelimNumbers

let parseXToY (x: string) (y: string) (s: string): RangeMap =
    let matches = Regex.Match(s, $"{x}-to-{y}\smap:([\d\s]+)")
    matches.Groups[1].Value |> (parseMapFromLines false)

let parseXToYReverse (x: string) (y: string) (s: string): RangeMap =
    let matches = Regex.Match(s, $"{x}-to-{y}\smap:([\d\s]+)")
    matches.Groups[1].Value |> (parseMapFromLines true)

let lowestLocationNumber (input: string) : int64 =
    let seeds = parseSeeds input
    let seedsToSoil = parseXToY "seed" "soil" input
    let soilToFert = parseXToY "soil" "fertilizer" input
    let fertToWater = parseXToY "fertilizer" "water" input
    let waterToLight = parseXToY "water" "light" input
    let lightToTemp = parseXToY "light" "temperature" input
    let tempToHum = parseXToY "temperature" "humidity" input
    let humToLoc = parseXToY "humidity" "location" input

    let seedToLoc: int64 -> int64 =
        seedsToSoil.get
        >> soilToFert.get
        >> fertToWater.get
        >> waterToLight.get
        >> lightToTemp.get
        >> tempToHum.get
        >> humToLoc.get

    seeds |> List.map seedToLoc |> List.min


let parseSeedRanges (input: string) : NumRange seq =
    let matches = Regex.Match(input, @"seeds:([\d\s]+)")
    let numbers = matches.Groups[1].Value |> parseSpaceDelimNumbers

    seq {
        for (start, len) in
            Seq.pairwise numbers
            |> Seq.indexed
            |> Seq.filter (fun (i, _) -> i % 2 = 0)
            |> Seq.map snd do
            yield! seq { { first = start; last = (start + len) } }
    }

let seedInRange (seed: int64) (ranges: NumRange list): bool =
    ranges
    |> List.exists (fun range -> seed >= range.first && seed <= range.last)
    //
    // let rec check(ranges: NumRange list) =
    //     match ranges with
    //     | [] -> false
    //     | range :: remaining ->
    //         if seed >= range.first && seed <= range.last then
    //             true
    //         else
    //             check remaining
    // check ranges

let lowestLocationNumberForSeedRangesParallel (input: string) : int64 =
    let seeds = parseSeedRanges input
    let seedsToSoil = parseXToY "seed" "soil" input 
    let soilToFert = parseXToY "soil" "fertilizer" input
    let fertToWater = parseXToY "fertilizer" "water" input
    let waterToLight = parseXToY "water" "light" input
    let lightToTemp = parseXToY "light" "temperature" input
    let tempToHum = parseXToY "temperature" "humidity" input
    let humToLoc = parseXToY "humidity" "location" input

    let seedToLoc: int64 -> int64 =
        seedsToSoil.get
        >> soilToFert.get
        >> fertToWater.get
        >> waterToLight.get
        >> lightToTemp.get
        >> tempToHum.get
        >> humToLoc.get
    
    let seedsFromRange (seedRange: NumRange): int64 seq =
        seq {
            for i in seedRange.first .. seedRange.last -> i
        }
    
    // let seedValues =
    //     seeds
    //     |> Seq.collect seedsFromRange
    
    let mutable minValue = Int64.MaxValue
    let lockObj = obj()
    
    Parallel.ForEach(seeds, fun (sr: NumRange) ->
        sr
        |> seedsFromRange
        |> Seq.iter (fun seed ->
            let result = seedToLoc seed
            Monitor.Enter(lockObj)
            try
                if result < minValue then
                    minValue <- result
            finally
                Monitor.Exit(lockObj)
            )
    ) |> ignore

    minValue



/// The idea is to reverse all the mappings and check whether
/// locations correspond to a seed which is in any of the seed ranges.
/// The first location for which the seed is present in the ranges
/// is the answer.
/// Inspired by:
/// https://github.com/werner77/AdventOfCode/blob/master/src/main/kotlin/com/behindmedia/adventofcode/year2023/day5/Day5.kt
let lowestLocationNumberForSeedRanges (input: string) : int64 =
    let seeds = parseSeedRanges input |> Seq.toList
    
    let soilToSeed = parseXToYReverse "seed" "soil" input
    let fertToSoil = parseXToYReverse "soil" "fertilizer" input
    let waterToFert = parseXToYReverse "fertilizer" "water" input
    let lightToWater = parseXToYReverse "water" "light" input
    let tempToLight = parseXToYReverse "light" "temperature" input
    let humToTemp = parseXToYReverse "temperature" "humidity" input
    let locToHum = parseXToYReverse "humidity" "location" input

    let locToSeed: int64 -> int64 =
        locToHum.get
        >> humToTemp.get
        >> tempToLight.get
        >> lightToWater.get
        >> waterToFert.get
        >> fertToSoil.get
        >> soilToSeed.get
    
    let mutable loc = 0L
    let mutable found = false
    let mutable result = -1L
    while not found do
        let seed = locToSeed loc
        let foundSeed = seedInRange seed seeds
        if foundSeed then
            result <- loc
            found <- true
        else
            loc <- loc + 1L
    result

/// Similar logic as above but with async operations.
let lowestLocationNumberForSeedRangesAsync (input: string) : int64 =
    let seeds = parseSeedRanges input |> Seq.toList
    
    let soilToSeed = parseXToYReverse "seed" "soil" input
    let fertToSoil = parseXToYReverse "soil" "fertilizer" input
    let waterToFert = parseXToYReverse "fertilizer" "water" input
    let lightToWater = parseXToYReverse "water" "light" input
    let tempToLight = parseXToYReverse "light" "temperature" input
    let humToTemp = parseXToYReverse "temperature" "humidity" input
    let locToHum = parseXToYReverse "humidity" "location" input

    let locToSeed: int64 -> int64 =
        locToHum.get
        >> humToTemp.get
        >> tempToLight.get
        >> lightToWater.get
        >> waterToFert.get
        >> fertToSoil.get
        >> soilToSeed.get
    
    let loc = ref 0L
    let processorCount = System.Environment.ProcessorCount
    
    let tasks =
        [ for _ in 1 .. processorCount ->
            async {
                let mutable current = 0L
                let mutable found = false
                while not found do
                    current <- Interlocked.Increment(loc) - 1L
                    let seed = locToSeed current
                    if seedInRange seed seeds then
                        found <- true
                return current
            } ]
    Async.Parallel(tasks)
    |> Async.RunSynchronously
    |> Array.min // all while loops have to conclude with some location value, and we take the min of those values.