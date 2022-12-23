module Day23

open System
open System.IO

let parseLine (y, s: string) =
    s.ToCharArray()
    |> Array.indexed
    |> Array.choose (fun (x, c) -> if c = '#' then Some (x, y) else None)

let allElves =
    File.ReadAllLines(@"input\day23.txt")
    |> Array.indexed
    |> Array.collect parseLine

type Grid =
    {
        MinX: int
        MaxX: int
        MinY: int
        MaxY: int
        Grid: int[,]
    }

let toGrid positions =
    if Array.isEmpty positions then
        {
            MinX = 0
            MaxX = 0
            MinY = 0
            MaxY = 0
            Grid = Array2D.create 1 1 0
        }
    else
        let minX = positions |> Array.minBy fst |> fst
        let maxX = positions |> Array.maxBy fst |> fst
        let minY = positions |> Array.minBy snd |> snd
        let maxY = positions |> Array.maxBy snd |> snd

        let grid = Array2D.create (1 + maxX - minX) (1 + maxY - minY) 0

        positions
        |> Array.iter (fun (x, y) -> grid.[x - minX, y - minY] <- grid.[x - minX, y - minY] + 1)

        {
            MinX = minX
            MaxX = maxX
            MinY = minY
            MaxY = maxY
            Grid = grid
        }

let populated grid (x, y) =
    if x < grid.MinX || x > grid.MaxX || y < grid.MinY || y > grid.MaxY then
        0
    else
        grid.Grid.[x - grid.MinX, y - grid.MinY]

type Direction = North | South | West | East

let nextDir dir =
    match dir with
    | North -> South
    | South -> West
    | West -> East
    | East -> North

let go dir x y =
    match dir with
    | North -> (x, y - 1)
    | South -> (x, y + 1)
    | West -> (x - 1, y)
    | East -> (x + 1, y)

type State =
    {
        Elves: (int * int) []
        FirstDirection: Direction
        DidMove: bool
    }

let processRound state =
    let elfGrid = toGrid state.Elves
    let empty x y = populated elfGrid (x, y) = 0

    let canGoNorth x y = empty (x - 1) (y - 1) && empty x (y - 1) && empty (x + 1) (y - 1)
    let canGoSouth x y = empty (x - 1) (y + 1) && empty x (y + 1) && empty (x + 1) (y + 1)
    let canGoWest x y = empty (x - 1) (y - 1) && empty (x - 1) y && empty (x - 1) (y + 1)
    let canGoEast x y = empty (x + 1) (y - 1) && empty (x + 1) y && empty (x + 1) (y + 1)
    let alone x y = canGoNorth x y && canGoSouth x y && empty (x - 1) y && empty (x + 1) y

    let canGo dir x y =
        match dir with
        | North -> canGoNorth x y
        | South -> canGoSouth x y
        | West -> canGoWest x y
        | East -> canGoEast x y
    
    let dir1 = state.FirstDirection
    let dir2 = nextDir dir1
    let dir3 = nextDir dir2
    let dir4 = nextDir dir3

    let elfProposals =
        state.Elves 
        |> Array.map (fun (x, y) ->
            let proposal =
                if alone x y then None
                elif canGo dir1 x y then go dir1 x y |> Some
                elif canGo dir2 x y then go dir2 x y |> Some
                elif canGo dir3 x y then go dir3 x y |> Some
                elif canGo dir4 x y then go dir4 x y |> Some
                else None
            (x, y), proposal)
    
    let proposals =
        elfProposals
        |> Array.choose snd
    
    let proposalGrid = toGrid proposals
    let mutable didMove = false

    let newElves =
        elfProposals
        |> Array.map (fun ((x, y), proposalOpt) ->
            match proposalOpt with
            | Some proposal ->
                if populated proposalGrid proposal > 1 then
                    (x, y)
                else
                    didMove <- true
                    proposal
            | None -> (x, y))
    
    { Elves = newElves; FirstDirection = dir2; DidMove = didMove }

let part1() =
    let mutable state = { Elves = allElves; FirstDirection = North; DidMove = true }
    
    for i in 1 .. 10 do
        state <- processRound state
    
    let grid = toGrid state.Elves
    let gridSize = (1 + grid.MaxX - grid.MinX) * (1 + grid.MaxY - grid.MinY)
    let numElves = state.Elves.Length

    let numEmpty = gridSize - numElves

    printfn "Number of empty spaces: %i" numEmpty

let part2() =
    let mutable state = { Elves = allElves; FirstDirection = North; DidMove = true }
    
    let mutable moves = 0

    while state.DidMove do
        state <- processRound state
        moves <- moves + 1
    
    printfn "Number of rounds: %i" moves
