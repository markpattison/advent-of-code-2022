module Day4

open System
open System.IO

let splitOn (c: char) (s: string) =
    match s.Split(c) with
    | [| s1; s2 |] -> s1, s2
    | _ -> failwith "error"    

let fromLine (s: string) =
    if String.IsNullOrWhiteSpace(s) then
        None
    else
        let sections1, sections2 = splitOn ',' s

        let min1, max1 = splitOn '-' sections1
        let min2, max2 = splitOn '-' sections2

        Some ((int min1, int max1), (int min2, int max2))

let sectionPairs =
    File.ReadAllLines(@"input\day4.txt")
    |> Array.choose fromLine
    |> Array.toList

let part1() =
    let numberContaining =
        sectionPairs
        |> List.filter (fun ((min1, max1), (min2, max2)) -> (min1 <= min2 && max1 >= max2) || (min2 <= min1 && max2 >= max1))
        |> List.length

    printfn "Number of pairs where one contains the other: %i" numberContaining

let part2() =
    let numberOverlapping =
        sectionPairs
        |> List.filter (fun ((min1, max1), (min2, max2)) -> (max1 >= min2 && min1 <= max2))
        |> List.length

    printfn "Number of overlapping pairs: %i" numberOverlapping
