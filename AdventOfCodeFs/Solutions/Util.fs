module Solutions.Util

open System.Runtime.CompilerServices

open System

let (|HeadTail|Empty|) (s: string) =
    if s |> String.IsNullOrEmpty then
        Empty
    else
        HeadTail(s[0], s[1..])

let stringToLines (input: string) : string list =
    input.Split("\n", StringSplitOptions.TrimEntries) |> Array.toList

[<Extension>]
type StringExtensions =
    [<Extension>]
    static member inline Reverse(this: string) =
        let rev = this.ToCharArray() |> Array.rev
        String.Join("", rev)
