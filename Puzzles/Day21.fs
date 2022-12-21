module Day21

open System
open System.IO

type Operation = Add | Subtract | Multiply | Divide

let evaluate op v1 v2 =
    match op with
    | Add -> v1 + v2
    | Subtract -> v1 - v2
    | Multiply -> v1 * v2
    | Divide -> v1 / v2

type Yell =
    | Value of float
    | Operation of Operation * string * string
    | Equals of string * string
    | Human

let toMonkey (s: string) =
    let name = s.Substring(0, 4)
    let yellString = s.Substring(6)
    
    let yell =
        if yellString.Length <= 10 then
            Value (float yellString)
        else
            let name1 = yellString.Substring(0, 4)
            let name2 = yellString.Substring(7, 4)
            match yellString.[5] with
            | '+' -> Operation (Add, name1, name2)
            | '-' -> Operation (Subtract, name1, name2)
            | '*' -> Operation (Multiply, name1, name2)
            | '/' -> Operation (Divide, name1, name2)
            | _ -> failwith "error"
    
    name, yell

let allMonkeys =
    File.ReadAllLines(@"input\day21.txt")
    |> Array.map toMonkey

let part1() =
    let mutable monkeys = allMonkeys |> Map.ofArray
    let mutable foundRoot = false

    while not foundRoot do
        let updates =
            monkeys
            |> Map.toSeq
            |> Seq.choose (fun (name, yell) ->
                match yell with
                | Value _ | Equals _ | Human -> None
                | Operation (op, name1, name2) ->
                    match monkeys.[name1], monkeys.[name2] with
                    | Value v1, Value v2 -> Some (name, Value (evaluate op v1 v2))
                    | _ -> None
                )
        updates
        |> Seq.iter (fun (name, yell) ->
            monkeys <- Map.add name yell monkeys)
        
        match monkeys.["root"] with
        | Value v -> foundRoot <- true
        | _ -> ()
    
    let root =
        match monkeys.["root"] with
        | Value v -> v
        | _ -> failwith "error"

    printfn "Root yell: %.5f" root

let part2() =
    let mutable monkeys = allMonkeys |> Map.ofArray

    let newRoot =
        match monkeys.["root"] with
        | Operation (_, name1, name2) -> Equals (name1, name2)
        | _ -> failwith "error"
    
    monkeys <- Map.add "root" newRoot monkeys
    monkeys <- Map.add "humn" Human monkeys

    let mutable anyUpdates = true

    while anyUpdates do
        let updates =
            monkeys
            |> Map.toSeq
            |> Seq.choose (fun (name, yell) ->
                match yell with
                | Value _ | Equals _ | Human -> None
                | Operation (op, name1, name2) ->
                    match monkeys.[name1], monkeys.[name2] with
                    | Value v1, Value v2 -> Some (name, Value (evaluate op v1 v2))
                    | _ -> None
                )
        updates
        |> Seq.iter (fun (name, yell) ->
            monkeys <- Map.add name yell monkeys)
        
        if Seq.isEmpty updates then anyUpdates <- false

    let rootValue, rootUnknown =
        match monkeys.["root"] with
        | Equals (name1, name2) ->
            match monkeys.[name1], monkeys.[name2] with
            | Value _, Value _ -> failwith "error"
            | Value v, _ -> v, name2
            | _, Value v -> v, name1
            | _ -> failwith "error"
        | _ -> failwith "error"

    let mutable toSolveFor = rootUnknown
    let mutable toSolveValue = rootValue
    let mutable foundHuman = false

    while not foundHuman do
        let solveValue, solveUnknown =
            match monkeys.[toSolveFor] with
            | Value _ | Equals _ -> failwith "error"
            | Operation (op, name1, name2) ->
                match op, monkeys.[name1], monkeys.[name2] with
                | _, Value _, Value _ -> failwith "error"
                | Add, Value v, _ -> toSolveValue - v, name2
                | Add, _, Value v -> toSolveValue - v, name1
                | Subtract, Value v, _ -> v - toSolveValue, name2
                | Subtract, _, Value v -> v + toSolveValue, name1
                | Multiply, Value v, _ -> toSolveValue / v, name2
                | Multiply, _, Value v -> toSolveValue / v, name1
                | Divide, Value v, _ -> v / toSolveValue, name2
                | Divide, _, Value v -> v * toSolveValue, name1                
                | _ -> failwith "error"
            | Human ->
                foundHuman <- true
                toSolveValue, toSolveFor
        
        toSolveFor <- solveUnknown
        toSolveValue <- solveValue
   
    printfn "Human number: %.5f" toSolveValue
