module Day22

open System
open System.IO

type Tile = Open | Wall | Wrap
type Instruction = Forward of int | Right | Left
type Facing = North | East | South | West

type State =
    {
        X: int
        Y: int
        Facing: Facing
    }

let parseLine (s: string) =
    s.ToCharArray()
    |> Array.map (fun c ->
        match c with
        | '.' -> Open
        | '#' -> Wall
        | ' ' -> Wrap
        | _ -> failwith "error")

let rec parseInstructions (c: char []) =
    if Array.isEmpty c then
        []
    elif c.[0] = 'L' then
        Left :: parseInstructions (Array.skip 1 c)
    elif c.[0] = 'R' then
        Right :: parseInstructions (Array.skip 1 c)
    else
        let mutable digits = 0
        while digits < c.Length && Char.IsDigit(c.[digits]) do
            digits <- digits + 1
        Forward (String(Array.take digits c) |> int) :: parseInstructions (Array.skip digits c)

let fullMap, instructions =
    let allLines = File.ReadAllLines(@"input\day22.txt")

    let map =
        allLines
        |> Array.take (allLines.Length - 2)
        |> Array.map parseLine
    
    let instructions =
        allLines
        |> Array.last
        |> fun s -> s.ToCharArray()
        |> parseInstructions
    
    map, instructions

let turnLeft f =
    match f with
    | North -> West
    | East -> North
    | South -> East
    | West -> South

let turnRight f =
    match f with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let dxy f =
    match f with
    | North -> (0, -1)
    | East -> (1, 0)
    | South -> (0, 1)
    | West -> (-1, 0)

let tileAt (map: Tile [][]) x y =
    if y < 0 || y >= map.Length || x < 0 || x >= map.[y].Length then
        Wrap
    else
        map.[y].[x]

let move (map: Tile [][]) wrapping state instruction : State =
    match instruction with
    | Left ->
        { state with Facing = turnLeft state.Facing }
    | Right ->
        { state with Facing = turnRight state.Facing }
    | Forward steps ->
        let mutable facing = state.Facing
        let mutable x = state.X
        let mutable y = state.Y
        let mutable stepsToGo = steps

        while stepsToGo > 0 do
            let dx, dy = dxy facing
            let preWrapX, preWrapY = x + dx, y + dy
            match tileAt map preWrapX preWrapY with
            | Wall ->
                stepsToGo <- 0
            | Open ->
                x <- preWrapX
                y <- preWrapY
                stepsToGo <- stepsToGo - 1
            | Wrap ->
                let wrapX, wrapY, newFacing = wrapping map x y facing

                match tileAt map wrapX wrapY with
                | Wall -> stepsToGo <- 0
                | Open ->
                    x <- wrapX
                    y <- wrapY
                    facing <- newFacing
                    stepsToGo <- stepsToGo - 1
                | Wrap -> failwith "error"
        
        { X = x; Y = y; Facing = facing }

let facingCode f =
    match f with
    | North -> 3
    | East -> 0
    | South -> 1
    | West -> 2

let wrapFrom (map: Tile [][]) x y facing =
    let dx, dy = dxy facing

    let mutable wrapX = x + dx
    let mutable wrapY = y + dy
    let mutable foundWrap = false

    while not foundWrap do
        wrapX <- wrapX - dx
        wrapY <- wrapY - dy
        if tileAt map wrapX wrapY = Wrap then foundWrap <- true

    (wrapX + dx, wrapY + dy, facing)

type Square = A | B | C | D | E | F

let square x y =
    match x / 50, y / 50 with
    | 1, 0 -> A
    | 2, 0 -> B
    | 1, 1 -> C
    | 0, 2 -> E
    | 1, 2 -> D
    | 0, 3 -> F
    | _ -> failwith "error"

let rec wrapCube _ x y facing =
    let sq = square x y
    let sqX, sqY = x % 50, y % 50
    let newX, newY, newF =
        match sq, facing with
        | A, North -> 0, 150 + sqX, East
        | A, West -> 0, 149 - sqY, East
        | B, North -> sqX, 199, North
        | B, East -> 99, 149 - sqY, West
        | B, South -> 99, 50 + sqX, West
        | C, East -> 100 + sqY, 49, North
        | C, West -> sqY, 100, South
        | D, East -> 149, 49 - sqY, West
        | D, South -> 49, 150 + sqX, West
        | E, North -> 50, 50 + sqX, East
        | E, West -> 50, 49 - sqY, East
        | F, East -> 50 + sqY, 149, North
        | F, South -> 100 + sqX, 0, South
        | F, West -> 50 + sqY, 0, South
        | _ -> failwith "error"

    (newX, newY, newF)

let part1() =
    let initialState =
        {
            X = Array.IndexOf(fullMap.[0], Open)
            Y = 0
            Facing = East
        }
    
    let finalState = List.fold (move fullMap wrapFrom) initialState instructions

    let password = 1000 * (finalState.Y + 1) + 4 * (finalState.X + 1) + facingCode finalState.Facing

    printfn "Password: %i" password

let part2() =
    let initialState =
        {
            X = Array.IndexOf(fullMap.[0], Open)
            Y = 0
            Facing = East
        }
    
    let finalState = List.fold (move fullMap wrapCube) initialState instructions

    let password = 1000 * (finalState.Y + 1) + 4 * (finalState.X + 1) + facingCode finalState.Facing

    printfn "Password: %i" password
