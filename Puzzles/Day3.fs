module Day3

open System
open System.IO

type Player1 = A | B | C
type Player2 = X | Y | Z

let fromLine (s: string) =
    if String.IsNullOrWhiteSpace(s) then
        None
    else
        let halfLength = s.Length / 2
        let compartment1 = s.[.. halfLength - 1]
        let compartment2 = s.[halfLength ..]
        Some (compartment1, compartment2)

let rucksacks =
    File.ReadAllLines(@"input\day3.txt")
    |> Array.choose fromLine
    |> Array.toList

let priority (c: char) =
    match int c with
    | upper when upper < 97 -> upper - 38
    | lower -> lower - 96

let part1() =
    let commonItems =
        rucksacks
        |> List.map (fun (s1, s2) -> s1 |> Seq.find (fun c -> Seq.contains c s2))

    let priorities =
        commonItems
        |> List.map priority

    let totalPriority = List.sum priorities

    printfn "Total priorities: %i" totalPriority

let part2() =
    let groups =
        rucksacks
        |> List.map (fun (s1, s2) -> s1 + s2)
        |> List.chunkBySize 3

    let badges =
        groups
        |> List.map (fun group ->
            match group with
            | [ items1; items2; items3 ] -> items1 |> Seq.find (fun c -> Seq.contains c items2 && Seq.contains c items3)
            | _ -> failwith "error")

    let priorities =
        badges
        |> List.map priority

    let totalPriority = List.sum priorities

    printfn "Total priorities: %i" totalPriority
