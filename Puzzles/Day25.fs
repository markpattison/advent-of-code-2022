module Day25

open System
open System.IO

let allLines =
    File.ReadAllLines(@"input\day25.txt")

let parseChar c =
    match c with
    | '=' -> -2L
    | '-' -> -1L
    | '0' -> 0L
    | '1' -> 1L
    | '2' -> 2L
    | _ -> failwith "error"

let parseSnafu (s: string) =
    s.ToCharArray()
    |> Array.rev
    |> Array.mapi (fun pos c -> (pown 5L pos) * parseChar c)
    |> Array.sum

let maxPow (x: int64) =
    let mutable p = 0
    let mutable fiveAcc = 2L

    while fiveAcc < x do
        p <- p + 1
        fiveAcc <- fiveAcc + 2L * pown 5L p
    p

let toSnafu (x: int64) =
    let mutable p = maxPow x
    let mutable reduced = x
    let mutable snafu = ""

    while p >= 0 do
        let fiveP = pown 5L p
        let digit =
            [| -2L, -2L * fiveP
               -1L, -1L * fiveP
               0L, 0L
               1L, fiveP
               2L, 2L * fiveP |]
            |> Array.minBy (fun (d, v) -> abs (reduced - v))
            |> fst
        let c =
            match digit with
            | -2L -> "="
            | -1L -> "-"
            | 0L -> "0"
            | 1L -> "1"
            | 2L -> "2"
            | _ -> failwith "error"
        snafu <- snafu + c
        p <- p - 1
        reduced <- reduced - digit * fiveP
    
    snafu

let part1() =
    let total =
        allLines
        |> Array.sumBy parseSnafu

    let snafu = toSnafu total

    printfn "SNAFU: %s" snafu

let part2() =
    ()
