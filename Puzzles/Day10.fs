module Day10

open System
open System.IO

type Instruction =
    | AddX of int
    | Noop

let toInstruction (s: string) =
    match s with
    | "noop" -> Noop
    | addx when addx.StartsWith("addx ") -> AddX (addx.Substring(5) |> int)
    | _ -> failwith "error"

let instructions =
    File.ReadAllLines(@"input\day10.txt")
    |> Array.map toInstruction

let unfold (_, xRegister) instruction =
    match instruction with
    | Noop -> [| xRegister |], xRegister
    | AddX n -> [| xRegister; xRegister |], (xRegister + n)

let xRegisterValues =
    instructions
    |> Array.scan unfold ([| |], 1)
    |> Array.collect fst

let part1() =
    let signalStrengths =
        xRegisterValues
        |> Array.mapi (fun i x -> (i + 1) * x)

    let sum =
        signalStrengths.[19] + signalStrengths.[59] + signalStrengths.[99]
        + signalStrengths.[139] + signalStrengths.[179] + signalStrengths.[219]

    printfn "Sum of signal strengths: %i" sum

let part2() =
    let pixels =
        xRegisterValues
        |> Array.mapi (fun i x ->
            let position = i % 40
            if abs (x - position) < 2 then '#' else '.')

    pixels
    |> Array.iteri (fun i c ->
        printf "%c" c
        if i % 40 = 39 then printfn "")
