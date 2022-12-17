module Day17

open System
open System.IO

type Jet = Left | Right
type Rock = Flat | Cross | L | Tall | Square

let allJets =
    File.ReadAllLines(@"input\day17.txt").[0].ToCharArray()
    |> Array.map (fun c ->
        match c with
        | '<' -> Left
        | '>' -> Right
        | _ -> failwith "error")

type State =
    {
        Chamber: bool [][]
        HighestRock: int
        RocksFallen: int
        JetsUsed: int
        Floor: int
    }

let rockToDrop rocksFallen =
    match rocksFallen % 5 with
    | 0 -> Flat
    | 1 -> Cross
    | 2 -> L
    | 3 -> Tall
    | 4 -> Square
    | _ -> failwith "error"

let nextJet (jets: Jet []) jetsUsed =
    jets.[jetsUsed % jets.Length]

let checkOccupied state x y =
    if x < 0 || x > 6 || y < state.Floor then
        true
    elif y > state.HighestRock then
        false
    else
        state.Chamber.[y - state.Floor].[x]

let blockedLeft state rock x y =
    let blocked = checkOccupied state
    match rock with
    | Flat -> blocked (x - 1)  y
    | Cross -> blocked x y || blocked (x - 1) (y + 1) || blocked x (y + 2)
    | L -> blocked (x - 1)  y || blocked (x + 1) (y + 1) || blocked (x + 1) (y + 2)
    | Tall -> blocked (x - 1) y || blocked (x - 1) (y + 1) || blocked (x - 1) (y + 2) || blocked (x - 1) (y + 3)
    | Square -> blocked (x - 1) y || blocked (x - 1) (y + 1)

let blockedRight state rock x y =
    let blocked = checkOccupied state
    match rock with
    | Flat -> blocked (x + 4)  y
    | Cross -> blocked (x + 2) y || blocked (x + 3) (y + 1) || blocked (x + 2) (y + 2)
    | L -> blocked (x+ 3)  y || blocked (x + 3) (y + 1) || blocked (x + 3) (y + 2)
    | Tall -> blocked (x + 1) y || blocked (x + 1) (y + 1) || blocked (x + 1) (y + 2) || blocked (x + 1) (y + 3)
    | Square -> blocked (x + 2) y || blocked (x + 2) (y + 1)

let blockedDown state rock x y =
    let blocked = checkOccupied state
    match rock with
    | Flat -> blocked x (y - 1) || blocked (x + 1) (y - 1) || blocked (x + 2) (y - 1) || blocked (x + 3) (y - 1)
    | Cross -> blocked x y || blocked (x + 1) (y - 1) || blocked (x + 2) y
    | L -> blocked x (y - 1) || blocked (x + 1) (y - 1) || blocked (x + 2) (y - 1)
    | Tall -> blocked x (y - 1)
    | Square -> blocked x (y - 1) || blocked (x + 1) (y - 1)

let highestPoint rock y =
    match rock with
    | Flat -> y
    | Cross -> y + 2
    | L -> y + 2
    | Tall -> y + 3
    | Square -> y + 1

let blockSquares (chamber: bool [][]) floor rock x y =
    let block x' y' = chamber.[y' - floor].[x'] <- true
    match rock with
    | Flat ->
        block x y
        block (x + 1) y
        block (x + 2) y
        block (x + 3) y
    | Cross ->
        block (x + 1) y
        block x (y + 1)
        block (x + 1) (y + 1)
        block (x + 2) (y + 1)
        block (x + 1) (y + 2)
    | L ->
        block x y
        block (x + 1) y
        block (x + 2) y
        block (x + 2) (y + 1)
        block (x + 2) (y + 2)
    | Tall ->
        block x y
        block x (y + 1)
        block x (y + 2)
        block x (y + 3)
    | Square ->
        block x y
        block (x + 1) y
        block x (y + 1)
        block (x + 1) (y + 1)

let dropRock jets state =
    let rock = rockToDrop state.RocksFallen

    let mutable x = 2
    let mutable y = state.HighestRock + 4
    let mutable jetsUsed = state.JetsUsed
    let mutable isFalling = true

    while isFalling do
        let jet = nextJet jets jetsUsed
        jetsUsed <- jetsUsed + 1

        match jet with
        | Left ->
            if blockedLeft state rock x y |> not then x <- x - 1
        | Right ->
            if blockedRight state rock x y |> not then x <- x + 1

        match blockedDown state rock x y with
        | true ->
            isFalling <- false
        | false ->
            y <- y - 1

    let mutable chamber = state.Chamber

    let highest = highestPoint rock y

    if highest > state.HighestRock then
        for i in 1 .. highest - state.HighestRock do
            chamber <- Array.append chamber [| (Array.create 7 false) |]

    blockSquares chamber state.Floor rock x y

    let floor =
        if chamber.Length > 50 then
            let remove = chamber.Length - 50
            chamber <- Array.skip remove chamber
            state.Floor + remove
        else
            state.Floor

    {
        JetsUsed = jetsUsed
        RocksFallen = state.RocksFallen + 1
        HighestRock = max state.HighestRock highest
        Chamber = chamber
        Floor = floor
    }

type CacheState =
    {
        RocksFallenMod5: int
        JetsUsedModTotal: int
        DepthsFromTop: int[]
    }

let toCacheState (jets: Jet []) state =
    let chamberHeight = state.Chamber.Length
    let depths = Array.init 7 (fun x ->
        let mutable d = 0
        while not (d >= chamberHeight || state.Chamber.[chamberHeight - 1 - d].[x]) do
            d <- d + 1
        d)
    {
        RocksFallenMod5 = state.RocksFallen % 5
        JetsUsedModTotal = state.JetsUsed % jets.Length
        DepthsFromTop = depths
    }

let run initialState numRocks =
    let mutable state = initialState

    for i in 1 .. numRocks do
        state <- dropRock allJets state

    state

type Cycle =
    {
        Start: int
        Length: int
        Height: int
    }

let findCycle initialState =
    let mutable state = initialState
    let mutable cache : Map<CacheState, int * int> = Map.empty
    let mutable hasCycled = false
    let mutable cycle = { Start = 0; Length = 0; Height = 0 }

    while not hasCycled do
        state <- dropRock allJets state
        let cacheState = toCacheState allJets state
        match Map.tryFind cacheState cache with
        | None -> cache <- cache.Add (cacheState, (state.RocksFallen, state.HighestRock + 1))
        | Some (rocks, highest) ->
            hasCycled <- true
            cycle <- { Start = rocks; Length = state.RocksFallen - rocks; Height = state.HighestRock + 1 - highest }

    cycle, state

let part1() =
    let initialState =
        {
            Chamber = [||]
            HighestRock = -1
            RocksFallen = 0
            JetsUsed = 0
            Floor = 0
        }

    let finalState = run initialState 2022

    let heightOfRocks = finalState.HighestRock + 1

    printfn "Height of rocks: %i" heightOfRocks

let part2() =
    let state =
        {
            Chamber = [||]
            HighestRock = -1
            RocksFallen = 0
            JetsUsed = 0
            Floor = 0
        }

    let cycle, stateAfterCycle = findCycle state
    let heightAfterOneCycle = stateAfterCycle.HighestRock + 1

    let target = 1000000000000L

    let cyclesNeeded = (target - int64 cycle.Start) / (int64 cycle.Length) - 1L
    let remainingCycles = (target - int64 cycle.Start) % (int64 cycle.Length)

    let stateAfterRemaining = run stateAfterCycle (int remainingCycles)
    let remainingHeight = (stateAfterRemaining.HighestRock + 1) - heightAfterOneCycle

    let totalHeight =
        int64 heightAfterOneCycle
        + cyclesNeeded * (int64 cycle.Height)
        + (int64 remainingHeight)

    printfn "Total height: %i" totalHeight
