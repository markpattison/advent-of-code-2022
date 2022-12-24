module Day24

open System
open System.IO

type Map =
    {
        StartX: int
        GoalX: int
        SizeX: int
        SizeY: int
        NorthBlizzards: bool[,]
        EastBlizzards: bool[,]
        SouthBlizzards: bool[,]
        WestBlizzards: bool[,]
    }

let fullMap =
    let allLines = File.ReadAllLines(@"input\day24.txt")
    let startX = Array.IndexOf(allLines.[0].ToCharArray(), '.') - 1
    let goalX = Array.IndexOf((Array.last allLines).ToCharArray(), '.') - 1

    let blizzardLines =
        allLines.[1 .. allLines.Length - 2]
        |> Array.map (fun s -> s.ToCharArray().[1 .. s.Length - 2])

    let sizeX = blizzardLines.[0].Length
    let sizeY = blizzardLines.Length

    let n = Array2D.create sizeX sizeY false
    let e = Array2D.create sizeX sizeY false
    let s = Array2D.create sizeX sizeY false
    let w = Array2D.create sizeX sizeY false

    blizzardLines
    |> Array.iteri (fun y row ->
        row
        |> Array.iteri (fun x c ->
            match c with
            | '.' -> ()
            | '^' -> n.[x, y] <- true
            | '>' -> e.[x, y] <- true
            | 'v' -> s.[x, y] <- true
            | '<' -> w.[x, y] <- true
            | _ -> failwith "error"))

    {
        StartX = startX
        GoalX = goalX
        SizeX = sizeX
        SizeY = sizeY
        NorthBlizzards = n
        EastBlizzards = e
        SouthBlizzards = s
        WestBlizzards = w
    }

let modulo x m =
    let r = x % m
    if r < 0 then r + m else r

let isNorthBlizzard map time x y = map.NorthBlizzards.[x, modulo (y + time) map.SizeY]
let isEastBlizzard map time x y = map.EastBlizzards.[modulo (x - time) map.SizeX, modulo y map.SizeY]
let isSouthBlizzard map time x y = map.SouthBlizzards.[x, modulo (y - time) map.SizeY]
let isWestBlizzard map time x y = map.WestBlizzards.[modulo (x + time) map.SizeX, modulo y map.SizeY]

let isAnyBlizzard map time x y =
    isNorthBlizzard map time x y
    || isEastBlizzard map time x y 
    || isSouthBlizzard map time x y
    || isWestBlizzard map time x y

let isStart map (x, y) =
    x = map.StartX && y = -1

let isGoal map (x, y) =
    x = map.GoalX && y = map.SizeY

let possibleMoves map time (x, y) =
    if isStart map (x, y) then
        [|
            if not (isAnyBlizzard map (time + 1) x (y + 1)) then (x, y + 1)
            (x, y)
        |]
    elif isGoal map (x, y) then
        [|
            if not (isAnyBlizzard map (time + 1) x (y - 1)) then (x, y - 1)
            (x, y)
        |]
    else
        [|
            if x > 0 && not (isAnyBlizzard map (time + 1) (x - 1) y) then (x - 1, y)
            if x < map.SizeX - 1 && not (isAnyBlizzard map (time + 1) (x + 1) y) then (x + 1, y)
            if (y > 0  && not (isAnyBlizzard map (time + 1) x (y - 1))) || (y = 0 && x = map.StartX) then (x, y - 1)
            if (y < map.SizeY - 1 && not (isAnyBlizzard map (time + 1) x (y + 1))) || (y = map.SizeY - 1 && x = map.GoalX) then (x, y + 1)
            if not (isAnyBlizzard map (time + 1) x y) then (x, y)
        |]

let rec processOneLevel map time positions =
    positions
    |> Array.collect (possibleMoves map time)
    |> Array.distinct

let rec foundGoal map positions =
    Array.exists (isGoal map) positions
let rec foundStart map positions =
    Array.exists (isStart map) positions

let part1() =
    let mutable positions = [| (fullMap.StartX, -1) |]
    let mutable time = 0
    let mutable finished = false

    while not finished do
        positions <- processOneLevel fullMap time positions
        finished <- foundGoal fullMap positions
        time <- time + 1

    printfn "Minimum moves: %i" time

let part2() =
    let mutable positions = [| (fullMap.StartX, -1) |]
    let mutable time = 0
    let mutable finished = false

    while not finished do
        positions <- processOneLevel fullMap time positions
        finished <- foundGoal fullMap positions
        time <- time + 1

    positions <- [| (fullMap.GoalX, fullMap.SizeY) |]
    finished <- false

    while not finished do
        positions <- processOneLevel fullMap time positions
        finished <- foundStart fullMap positions
        time <- time + 1
    
    positions <- [| (fullMap.StartX, -1) |]
    finished <- false

    while not finished do
        positions <- processOneLevel fullMap time positions
        finished <- foundGoal fullMap positions
        time <- time + 1
    
    printfn "Minimum moves: %i" time
