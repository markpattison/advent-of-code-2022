module Day20

open System
open System.IO

let allNumbers =
    File.ReadAllLines(@"input\day20.txt")
    |> Array.map int64

let modulo x m =
    let r = x % m
    if r < 0L then r + m else r

let moveNumber (numbers: int64 []) (origOrdering: int []) origIndex =
    let index = Array.IndexOf(origOrdering, origIndex)
    let n = numbers.[index]

    let removed = Array.removeAt index numbers
    let removedOrdering = Array.removeAt index origOrdering

    let newIndex = modulo (int64 index + int64 n) (int64 removed.Length) |> int

    let inserted = Array.insertAt newIndex n removed
    let insertedOrdering = Array.insertAt newIndex origIndex removedOrdering

    inserted, insertedOrdering

let mix (numbers: int64 []) (origOrdering: int[]) =
    let mutable mixed = numbers
    let mutable ordering = origOrdering

    for i in 0 .. numbers.Length - 1 do
        let result = moveNumber mixed ordering i
        mixed <- fst result
        ordering <- snd result

    (mixed, ordering)

let mixOnce (numbers: int64 []) =
    let ordering = Array.init numbers.Length id
    mix numbers ordering |> fst

let mixMultiple (numbers: int64 []) numMixes =
    let mutable mixed = numbers
    let mutable ordering = Array.init numbers.Length id

    for i in 0 .. numMixes - 1 do
        let result = mix mixed ordering
        mixed <- fst result
        ordering <- snd result        

    mixed

let extractCoords (numbers: int64 []) =
    let indexZero = Array.IndexOf(numbers, 0L)
    let valAtIndex i = numbers.[i % numbers.Length]
    let v1000 = valAtIndex (indexZero + 1000)
    let v2000 = valAtIndex (indexZero + 2000)
    let v3000 = valAtIndex (indexZero + 3000)

    v1000 + v2000 + v3000

let part1() =
    let mixed = mixOnce allNumbers

    let sum = extractCoords mixed

    printfn "Sum: %i" sum

let part2() =
    let toMix = allNumbers |> Array.map (fun x -> x * 811589153L)
    
    let mixed = mixMultiple toMix 10

    let sum = extractCoords mixed

    printfn "Sum: %i" sum
    