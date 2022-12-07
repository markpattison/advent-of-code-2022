module Day7

open System
open System.IO

type CD = Root | Up | Down of string

type Command =
    | CD of CD
    | LS

type Listing =
    | DirListing of string
    | FileListing of string * int

type Token = Command of Command | Listing of Listing



let tokeniseCommand (s: string) =
    match s with
    | @"ls" -> LS
    | @"cd /" -> CD Root
    | @"cd .." -> CD Up
    | x when x.StartsWith("cd ") -> CD (Down (x.Substring(3)))
    | _ -> failwithf "error tokenising command: %s" s

let tokeniseEntry (s: string) =
    let split = s.Split(' ')
    match split with
    | [| "dir"; d |] -> DirListing d
    | [| size; name |] -> FileListing (name, int size)
    | _ -> failwithf "error tokenising entry: %s" s

let tokenise (s: string) =
    if s.StartsWith("$ ") then
        s.Substring(2) |> tokeniseCommand |> Command
    else
        s |> tokeniseEntry |> Listing

type ParsedCommand =
    | ParsedCD of CD
    | ParsedListing of Listing list

let parse tokens =
    let folder (revAcc, currentListing) token =
        match token, currentListing with
        | Command (CD cd), None -> (ParsedCD cd) :: revAcc, None
        | Command (CD cd), Some entries -> (ParsedCD cd) :: (ParsedListing entries) :: revAcc, None
        | Command LS, None -> revAcc, Some []
        | Command LS, Some _ -> failwith "error"
        | Listing _, None -> failwith "error"
        | Listing l, Some entries -> revAcc, Some (l :: entries)
    
    tokens
    |> List.fold folder ([], None)
    |> fun (revAcc, currentListing) ->
        match currentListing with
        | None -> revAcc
        | Some entries -> (ParsedListing entries) :: revAcc
    |> List.rev

type DirectoryEntry =
    | File of string * int
    | Directory of string * DirectoryEntry list

let rec updateTree (tree: DirectoryEntry list) path listing =
    match List.rev path with
    | [] ->
        listing
        |> List.map (fun l ->
            match l with
            | DirListing dir -> Directory (dir, [])
            | FileListing (name, size) -> File (name, size))
    | dirName :: remainingPath ->
        tree
        |> List.map (fun dirEntry ->
            match dirEntry with
            | File _ -> dirEntry
            | Directory (dir, entries) when dir = dirName -> Directory (dir, updateTree entries (List.rev remainingPath) listing)
            | Directory _ -> dirEntry)

let toTree parsedTokens =
    let folder (tree, currentPath) parsedCommand =
        match parsedCommand with
        | ParsedCD Root -> tree, []
        | ParsedCD Up -> tree, List.tail currentPath
        | ParsedCD (Down dir) -> tree, dir :: currentPath
        | ParsedListing listing -> updateTree tree currentPath listing, currentPath

    parsedTokens
    |> List.fold folder ([], [])
    |> fst

let tree =
    File.ReadAllLines(@"input\day7.txt")
    |> List.ofArray
    |> List.map tokenise
    |> parse
    |> toTree

let getRootSizeAndAllSizes tree =
    let mutable allDirSizes = []

    let rec totalSize (tree: DirectoryEntry list) =
        let total =
            tree
            |> List.sumBy (fun entry ->
                match entry with
                | File (_, size) -> size
                | Directory (_, dir) -> totalSize dir)
        allDirSizes <- total :: allDirSizes
        total

    let rootSize = totalSize tree
    rootSize, allDirSizes

let part1() =
    let _, allDirSizes = getRootSizeAndAllSizes tree

    let totalUpToLimit =
        allDirSizes
        |> List.filter (fun size -> size <= 100000)
        |> List.sum

    printfn "Total directories up to 100k: %i" totalUpToLimit

let part2() =
    let rootSize, allDirSizes = getRootSizeAndAllSizes tree

    let currentUnused = 70000000 - rootSize
    let needToDelete = 30000000 - currentUnused

    let smallestToDelete =
        allDirSizes
        |> List.filter (fun size -> size >= needToDelete)
        |> List.min

    printfn "Smallest directory to delete: %i" smallestToDelete
