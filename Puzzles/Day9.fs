module Day9

open System
open System.IO

type Direction = Right | Left | Up | Down
type Move = Direction * int
type Position = int * int
type State = Position [] // first entry is head

let toMove (s: string) : Move =
    let direction =
        match s.[0] with
        | 'R' -> Right
        | 'L' -> Left
        | 'U' -> Up
        | 'D' -> Down
        | _ -> failwith "error"
    let distance = s.Substring(2) |> int

    (direction, distance)

let moves =
    File.ReadAllLines(@"input\day9.txt")
    |> Array.map toMove

let expandMove (move: Move) =
    let dir, num = move
    Array.create num dir

let updateHead (position: Position) dir : Position =
    let hx, hy = position
    let dhx, dhy =
        match dir with
        | Right -> 1, 0
        | Left -> -1, 0
        | Up -> 0, 1
        | Down -> 0, -1

    (hx + dhx, hy + dhy)

let updateKnot (position: Position) (head: Position) =
    let x, y = position
    let hx, hy = head

    let dx, dy =
        match hx - x, hy - y with
        | 2, 0 -> 1, 0
        | -2, 0 -> -1, 0
        | 0, 2 -> 0, 1
        | 0, -2 -> 0, -1
        | 1, 2 | 2, 1 | 2, 2 -> 1, 1
        | 2, -1 | 1, -2 | 2, -2 -> 1, -1
        | -1, -2 | -2, -1 | -2, -2 -> -1, -1
        | -2, 1 | -1, 2 | -2, 2 -> -1, 1
        | a, b when abs a < 2 && abs b < 2 -> 0, 0
        | _ -> failwith "error"

    (x + dx, y + dy)

let updateStep (state: State) dir : State =
    let head = state.[0]
    let newHead = updateHead head dir

    let newState = Array.zeroCreate (state.Length)
    newState.[0] <- newHead

    for i in 1 .. state.Length - 1 do
        newState.[i] <- updateKnot state.[i] newState.[i - 1]

    newState

let totalPositionsVisitedByTail numKnots =
    let initialState = Array.create numKnots (0, 0)

    moves
    |> Array.collect expandMove
    |> Array.scan updateStep initialState
    |> Array.map Array.last
    |> Set.ofArray
    |> Set.count

let part1() =
    let positionsTailVisited = totalPositionsVisitedByTail 2

    printfn "Positions tail visited: %i" positionsTailVisited

let part2() =
    let positionsTailVisited = totalPositionsVisitedByTail 10

    printfn "Positions tail visited: %i" positionsTailVisited    
