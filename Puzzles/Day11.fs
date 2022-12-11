module Day11

open System
open System.IO

type Monkey =
    {
        Id: int
        Items: int64 []
        Operation: int64 -> int64
        TestDivisibleBy: int64
        ThrowToIfTrue: int
        ThrowToIfFalse: int
        Inspections: int64
    }

type State = Monkey []

let toMonkey (lines: string []) =
    {
        Id = lines.[0].Substring(0, lines.[0].Length - 1).Substring(7) |> int
        Items = lines.[1].Substring(17).Split(',') |> Array.map int64
        Operation =
            let f = lines.[2].Substring(19)
            match f with
            | "old * old" -> fun x -> x * x
            | mul when mul.StartsWith("old * ") ->
                let multiplier = mul.Substring(6) |> int64
                fun x -> x * multiplier
            | add when add.StartsWith("old + ") ->
                let adder = add.Substring(6) |> int64
                fun x -> x + adder
            | _ -> failwith "error"
        TestDivisibleBy = lines.[3].Substring(20) |> int64
        ThrowToIfTrue = lines.[4].Substring(28) |> int
        ThrowToIfFalse = lines.[5].Substring(29) |> int
        Inspections = 0
    }

let monkeys =
    File.ReadAllLines(@"input\day11.txt")
    |> Array.chunkBySize 7
    |> Array.map toMonkey

let addToMonkey (state: State) monkeyId item : State =
    state
    |> Array.map (fun m ->
        if m.Id = monkeyId then
            { m with Items = Array.append m.Items [| item |] }
        else m)

let processMonkey relief (state: State) monkeyId : State =
    let mutable interimState = state

    let monkey = state |> Array.find (fun m -> m.Id = monkeyId)

    monkey.Items
    |> Array.iter (fun item ->
        let inspectedItem = relief (monkey.Operation item)
        let destinationMonkey =
            if inspectedItem % monkey.TestDivisibleBy = 0L then monkey.ThrowToIfTrue else monkey.ThrowToIfFalse
        interimState <- addToMonkey interimState destinationMonkey inspectedItem)

    interimState
    |> Array.map (fun m ->
        if m.Id = monkeyId then
            { m with Items = [||]; Inspections = m.Inspections + (int64 monkey.Items.Length) }
        else
            m)

let processRound (state: State) relief : State =
    state
    |> Array.map (fun m -> m.Id)
    |> Array.sort
    |> Array.fold (processMonkey relief) state

let processRounds relief (state: State) n : State =
    let mutable interimState = state
    
    for _ in 1 .. n do
        interimState <- processRound interimState relief

    interimState

let part1() =
    let relief x = x / 3L
    let monkeyBusiness =
        processRounds relief monkeys 20
        |> Array.sortByDescending (fun m -> m.Inspections)
        |> fun arr -> arr.[0].Inspections * arr.[1].Inspections
    
    printfn "Monkey business: %i" monkeyBusiness

let part2() =
    let productOfDivisors =
        monkeys
        |> Array.map (fun m -> m.TestDivisibleBy)
        |> Array.fold (*) 1L
    let relief x = x % productOfDivisors
    let monkeyBusiness =
        processRounds relief monkeys 10000
        |> Array.sortByDescending (fun m -> m.Inspections)
        |> fun arr -> arr.[0].Inspections * arr.[1].Inspections
    
    printfn "Monkey business: %i" monkeyBusiness
