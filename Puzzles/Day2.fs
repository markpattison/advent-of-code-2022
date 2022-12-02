module Day2

open System
open System.IO

type Player1 = A | B | C
type Player2 = X | Y | Z

let fromLine (s: string) =
    if String.IsNullOrWhiteSpace(s) then
        None
    else
        let p1 = match s.[0] with 'A' -> A | 'B' -> B | 'C' -> C | _ -> failwith "error"
        let p2 = match s.[2] with 'X' -> X | 'Y' -> Y | 'Z' -> Z | _ -> failwith "error"
        Some (p1, p2)

let rounds =
    File.ReadAllLines(@"input\day2.txt")
    |> Array.choose fromLine
    |> Array.toList

let part1() =
    let scoreRound (p1, p2) =

        // A: rock, B: paper, C: scissors
        // X: rock, Y: paper, Z: scissors

        let shapeScore = match p2 with X -> 1 | Y -> 2 | Z -> 3
        let gameScore =
            match p1, p2 with
            | A, Y | B, Z | C, X -> 6
            | A, X | B, Y | C, Z -> 3
            | A, Z | B, X | C, Y -> 0
        shapeScore + gameScore

    let total = rounds |> List.sumBy scoreRound

    printfn "Total score: %i" total

let part2() =
    let scoreRound (p1, p2) =
        
        // A: rock, B: paper, C: scissors
        // X: lose, Y: draw, Z: win

        let shapeScore =
            match p1, p2 with
            | A, Y | B, X | C, Z -> 1 // choose rock
            | A, Z | B, Y | C, X -> 2 // choose paper
            | A, X | B, Z | C, Y -> 3 // choose scissors
        let gameScore = match p2 with X -> 0 | Y -> 3 | Z -> 6
        shapeScore + gameScore

    let total = rounds |> List.sumBy scoreRound

    printfn "Total score: %i" total
