module Day16

open System
open System.IO

type Valve =
    {
        Name: string
        FlowRate: int
        TunnelsTo: string[]
    }

let parseLine (s: string) =
    let name = s.Substring(6, 2)
    let semicolon = s.IndexOf(';')
    let flowRate = s.Substring(23, semicolon - 23) |> int
    let tunnels =
        s.Split(',')
        |> Array.map (fun t -> t.Substring(t.Length - 2))
    {
        Name = name
        FlowRate = flowRate
        TunnelsTo = tunnels
    }
let origValves =
    File.ReadAllLines(@"input\day16.txt")
    |> Array.map parseLine

type Environment =
    {
        StartValve: int
        DestinationValveIds: int []
        Distances: Map<int, int []>
        FlowRates: int []
    }

let valveDistances startValve (valves: Valve []) =
    let valveIds = valves |> Array.mapi (fun i v -> (v.Name, i)) |> Map.ofArray
    let numValves = valves.Length

    let tunnelsTo =
        valves
        |> Array.map (fun v -> v.TunnelsTo |> Array.map (fun t -> valveIds.[t]))

    let originValveIds = valves |> Array.filter (fun v -> v.FlowRate > 0 || v.Name = startValve) |> Array.map (fun v -> valveIds.[v.Name])
    let destinationValveIds = valves |> Array.filter (fun v -> v.FlowRate > 0) |> Array.map (fun v -> valveIds.[v.Name])

    let distancesFrom start =
        let mutable unvisited = [0 .. numValves - 1] |> Set.ofList
        let tentativeDistances = Array.init numValves (fun i -> if i = start then 0 else Int32.MaxValue)
        let mutable current = start
        let mutable finished = false

        while not finished do
            let currentDistance = tentativeDistances.[current]
            tunnelsTo.[current]
            |> Array.iter (fun t ->
                let distanceThroughCurrent = currentDistance + 1
                if distanceThroughCurrent < tentativeDistances.[t] then tentativeDistances.[t] <- distanceThroughCurrent)
            
            unvisited <- Set.remove current unvisited

            if unvisited.IsEmpty then
                finished <- true
            else
                current <-
                    unvisited
                    |> Set.toSeq
                    |> Seq.minBy (fun v -> tentativeDistances.[v])

        tentativeDistances

    let allDistances =
        originValveIds
        |> Array.map (fun i -> i, distancesFrom i)
        |> Map.ofArray

    {
        StartValve = valveIds.[startValve]
        DestinationValveIds = destinationValveIds
        Distances = allDistances
        FlowRates = valves |> Array.map (fun v -> v.FlowRate)
    }

type State =
    {
        RemainingMinutes: int
        ReleasedSoFar: int
        Location: int
        Unopened: int []
    }

let rec maxReleased environment state =
    if state.RemainingMinutes <= 2 || state.Unopened.Length = 0 then
        state.ReleasedSoFar
    else
        let location = state.Location
        let distancesFromHere = environment.Distances.[location]
        let canVisit = state.Unopened |> Array.filter (fun v -> distancesFromHere.[v] < state.RemainingMinutes - 1)

        if canVisit.Length = 0 then
            state.ReleasedSoFar
        else
            canVisit
            |> Array.map (fun dest ->
                maxReleased
                    environment
                    {
                        RemainingMinutes = state.RemainingMinutes - distancesFromHere.[dest] - 1
                        ReleasedSoFar = state.ReleasedSoFar + environment.FlowRates.[dest] * (state.RemainingMinutes - distancesFromHere.[dest] - 1)
                        Location = dest
                        Unopened = state.Unopened |> Array.except [| dest |]
                    })
            |> Array.max

let part1() =
    let environment = valveDistances "AA" origValves
    
    let initialState =
        {
            RemainingMinutes = 30
            ReleasedSoFar = 0
            Location = environment.StartValve
            Unopened = environment.DestinationValveIds
        }

    let sw = System.Diagnostics.Stopwatch()
    sw.Start()

    let maxPressure = maxReleased environment initialState

    printfn "Max pressure released: %i" maxPressure

    sw.Stop()
    printfn "MS: %i" sw.ElapsedMilliseconds

let maxElephant environment unopened =
    let elephantInitialState =
        {
            RemainingMinutes = 26
            ReleasedSoFar = 0
            Location = environment.StartValve
            Unopened = unopened
        }
    maxReleased environment elephantInitialState

let rec maxReleasedPlusElephant environment state =
    if state.RemainingMinutes <= 2 || state.Unopened.Length = 0 then
        state.ReleasedSoFar + maxElephant environment state.Unopened
    else
        let location = state.Location
        let distancesFromHere = environment.Distances.[location]
        let canVisit = state.Unopened |> Array.filter (fun v -> distancesFromHere.[v] < state.RemainingMinutes - 1)

        if canVisit.Length = 0 then
            state.ReleasedSoFar + maxElephant environment state.Unopened
        else
            canVisit
            |> Array.map (fun dest ->
                maxReleasedPlusElephant
                    environment
                    {
                        RemainingMinutes = state.RemainingMinutes - distancesFromHere.[dest] - 1
                        ReleasedSoFar = state.ReleasedSoFar + environment.FlowRates.[dest] * (state.RemainingMinutes - distancesFromHere.[dest] - 1)
                        Location = dest
                        Unopened = state.Unopened |> Array.except [| dest |]
                    })
            |> Array.max

let part2() =
    let environment = valveDistances "AA" origValves

    let initialState =
        {
            RemainingMinutes = 26
            ReleasedSoFar = 0
            Location = environment.StartValve
            Unopened = environment.DestinationValveIds
        }

    let sw = System.Diagnostics.Stopwatch()
    sw.Start()

    let maxPressure = maxReleasedPlusElephant environment initialState

    printfn "Max pressure released: %i" maxPressure

    sw.Stop()
    printfn "MS: %i" sw.ElapsedMilliseconds
