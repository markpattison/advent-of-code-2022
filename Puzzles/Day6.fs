module Day6

open System
open System.IO

let buffer =
    File.ReadAllLines(@"input\day6.txt").[0]

let findDifferentChars n (s: string) =
    s
    |> Seq.windowed n
    |> Seq.findIndex (fun block -> (Set.ofSeq block).Count = n)
    |> (fun i -> i + n)

let part1() =
    findDifferentChars 4 buffer |> printfn "Marker index: %i"

let part2() =
    findDifferentChars 14 buffer |> printfn "Marker index: %i"
