module Day1

open System
open System.IO

let calories =
    File.ReadAllLines(@"input\day1.txt")
    |> Array.map(fun s -> if String.IsNullOrWhiteSpace(s) then None else Some (System.Int32.Parse(s)))

let elfTotals =
    calories
    |> Array.fold (fun (accTotals, currentTotal) caloriesOpt ->
        match caloriesOpt with
        | None -> (currentTotal :: accTotals, 0)
        | Some cal -> (accTotals, currentTotal + cal)) ([], 0)
    |> fun (accTotals, currentTotal) -> if currentTotal > 0 then currentTotal :: accTotals else accTotals // include last elf
    |> List.rev

let part1() =
    List.max elfTotals
    |> printfn "Largest elf total: %i"

let part2() =
    elfTotals
    |> List.sortDescending
    |> List.take 3
    |> List.sum
    |> printfn "Total of three largest elf totals: %i"
