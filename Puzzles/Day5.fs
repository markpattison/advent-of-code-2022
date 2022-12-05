module Day5

open System
open System.IO

let allLines =
    File.ReadAllLines(@"input\day5.txt")
    |> Array.toList

let numCrateLines =
    allLines
    |> List.findIndex (fun line -> line.[1] = '1')

let crateContents =
    let numCrates = ((allLines.Item numCrateLines).Length + 2) / 4
    
    let crates : char list [] = Array.create numCrates []

    for crate in 0 .. numCrates - 1 do
        for line in (numCrateLines - 1) .. -1 .. 0 do
            let char = (allLines.Item line).[1 + 4 * crate]
            if char <> ' ' then
                crates.[crate] <- char :: crates.[crate]

    crates

let moves =
    allLines
    |> List.skip (numCrateLines + 2)
    |> List.map (fun line ->
        let posNum = 5
        let fromTextPos = line.IndexOf("from")
        let lengthNum = fromTextPos - 6
        let posFrom = fromTextPos + 5
        let posTo = fromTextPos + 10

        let num = line.Substring(posNum, lengthNum) |> int
        let from = line.Substring(posFrom, 1) |> int
        let to' = line.Substring(posTo, 1) |> int
        
        (num, from, to'))

let moveOne (state: char list []) from to' =
    let toMove, remaining =
        match state.[from - 1] with
        | x :: xs -> x, xs
        | [] -> failwith "error"

    state.[from - 1] <- remaining
    state.[to' - 1] <- toMove :: state.[to' - 1]

let moveOneByOne state (num, from, to') =
    for _ in 1 .. num do
        moveOne state from to'

let moveMultiple (state: char list []) (num, from, to') =
    let toMove, remaining = state.[from - 1] |> List.splitAt num

    state.[from - 1] <- remaining
    state.[to' - 1] <- toMove @ state.[to' - 1]

let part1() =
    let state = Array.copy crateContents

    moves
    |> List.iter (moveOneByOne state)

    let topCrates =
        state
        |> Array.map (fun stack -> stack.Head)
        |> System.String

    printfn "%s" topCrates

let part2() =
    let state = Array.copy crateContents

    moves
    |> List.iter (moveMultiple state)

    let topCrates =
        state
        |> Array.map (fun stack -> stack.Head)
        |> System.String

    printfn "%s" topCrates
