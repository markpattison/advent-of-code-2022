module Day13

open System
open System.IO

type Token =
    | StartList
    | EndList
    | Value of int

type ListEntry =
    | PacketInt of int
    | PacketList of ListEntry list

let rec tokenise acc (s: string) =
    match s with
    | "" -> List.rev acc
    | x when x.StartsWith('[') -> tokenise (StartList :: acc) (x.Substring(1))
    | x when x.StartsWith(']') -> tokenise (EndList :: acc) (x.Substring(1))
    | x when x.StartsWith(',') -> tokenise acc (x.Substring(1))
    | _ ->
        let value, remaining =
            if s.Length > 1 && Char.IsAsciiDigit(s.[1]) then
                s.Substring(0, 2) |> int, s.Substring(2)
            else
                s.Substring(0, 1) |> int, s.Substring(1)
        tokenise (Value value :: acc) remaining

let rec parseList tokens =
    let rec innerParse currentItems tokens =
        match tokens with
        | [] -> currentItems, []
        | Value v :: remaining -> innerParse (PacketInt v :: currentItems) remaining
        | StartList :: remaining ->
            let items, thenRemaining = innerParse [] remaining
            innerParse (PacketList (List.rev items) :: currentItems) thenRemaining
        | EndList :: remaining ->
            currentItems, remaining
    
    innerParse [] tokens
    |> fst
    |> List.head

let parseLine (s: string) =
    tokenise [] s
    |> parseList

let parseChunk (arr: string []) =
    parseLine arr.[0], parseLine arr.[1]

let pairs =
    File.ReadAllLines(@"input\day13.txt")
    |> Array.chunkBySize 3
    |> Array.map parseChunk

type Order = Correct | Incorrect | Unsure

let rec inCorrectOrder packet1 packet2 =
    match packet1, packet2 with
    | PacketInt v1, PacketInt v2 ->
        if v1 < v2 then Correct elif v1 > v2 then Incorrect else Unsure
    | PacketInt v1, PacketList l2 -> listsInCorrectOrder [ PacketInt v1 ] l2
    | PacketList l1, PacketInt v2 -> listsInCorrectOrder l1 [ PacketInt v2 ]
    | PacketList l1, PacketList l2 -> listsInCorrectOrder l1 l2

and listsInCorrectOrder list1 list2 =
    match list1, list2 with
    | [], [] -> Unsure
    | head1 :: tail1, head2 :: tail2 ->
        match inCorrectOrder head1 head2 with
        | Unsure -> listsInCorrectOrder tail1 tail2
        | order -> order
    | [], _ -> Correct
    | _, [] -> Incorrect

let part1() =
    let sumIndices =
        pairs
        |> Array.mapi (fun i (packet1, packet2) -> i + 1, inCorrectOrder packet1 packet2)
        |> Array.filter (fun (_, order) -> order = Correct)
        |> Array.sumBy fst
    
    printfn "Sum of indices: %i" sumIndices

let part2() =
    let divider2 = PacketList [ PacketList [ PacketInt 2 ] ]
    let divider6 = PacketList [ PacketList [ PacketInt 6 ] ]

    let allPackets =
        pairs
        |> Array.collect (fun (p1, p2) -> [| p1; p2 |])
        |> Array.append [| divider2; divider6 |]
    
    let comparer p1 p2 =
        match inCorrectOrder p1 p2 with
        | Correct -> -1
        | Incorrect -> 1
        | Unsure -> failwith "error"

    let sorted = allPackets |> Array.sortWith comparer

    let indexDivider2 = 1 + Array.IndexOf(sorted, divider2)
    let indexDivider6 = 1 + Array.IndexOf(sorted, divider6)
    
    printfn "Decoder key: %i" (indexDivider2 * indexDivider6)
