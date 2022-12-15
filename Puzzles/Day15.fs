module Day15

open System
open System.IO

type Position = int * int

type Sensor =
    {
        Position: Position
        ClosestBeacon: Position
        Distance: int
    }

let distance (p1: Position) (p2: Position) =
    abs (fst p1 - fst p2) + abs (snd p1 - snd p2)

let parseLine (s: string) =
    let firstComma = s.IndexOf(',')
    let colon = s.IndexOf(':')
    let sensorX = s.Substring(12, firstComma - 12) |> int
    let sensorY = s.Substring(firstComma + 4, colon - firstComma - 4) |> int

    let isAt = s.IndexOf("is at")
    let secondComma = s.IndexOf(',', firstComma + 1)
    let beaconX = s.Substring(isAt + 8, secondComma - isAt - 8) |> int
    let beaconY = s.Substring(secondComma + 4) |> int

    let position = sensorX, sensorY
    let closestBeacon = beaconX, beaconY
    {
        Position = position
        ClosestBeacon = closestBeacon
        Distance = distance position closestBeacon
    }

let sensors =
    File.ReadAllLines(@"input\day15.txt")
    |> Array.map parseLine

type InclusiveRange =
    {
        Min: int
        Max: int
    }

type OrderedInclusiveRanges =
    {
        Ranges: InclusiveRange [] // ordered by Min
    }

let merge r1 r2 =
    let ra, rb = if r1.Min > r2.Min then r2, r1 else r1, r2 // now ra.Min >= rb.Min

    if rb.Min > ra.Max + 1 then
        [| ra; rb |] // do not overlap
    else
        let joint = { Min = ra.Min; Max = max ra.Max rb.Max}
        [| joint |]

let mergeOrdered oir r =
    if oir.Ranges.Length = 0 then
        { Ranges = [| r |] }
    else
        let allButLast = oir.Ranges |> Array.take (oir.Ranges.Length - 1)
        let lastRange = oir.Ranges |> Array.last
        let merged = merge lastRange r
        { Ranges = Array.append allButLast merged }

let rangeSize r = 1 + r.Max - r.Min
let orderedRangeSize oir = oir.Ranges |> Array.sumBy rangeSize

let rangeContains x r = x >= r.Min && x <= r.Max
let orderedRangeContains x oir = Array.exists (rangeContains x) oir.Ranges

let rangeIsContainedByRange inner outer = inner.Min >= outer.Min && inner.Max <= outer.Max
let orderedRangeContainsRange oir r = Array.exists (rangeIsContainedByRange r) oir.Ranges

let getRanges y =
    sensors
    |> Array.choose (fun s ->
        let sx, sy = s.Position
        let yDist = abs (y - sy)
        if yDist > s.Distance then
            None
        else
            let size = s.Distance - yDist
            Some { Min = sx - size; Max = sx + size })
    |> Array.sortBy (fun r -> r.Min)
    |> Array.fold mergeOrdered { Ranges = [||] }

let part1() =
    let y = 2000000

    let ranges = getRanges y

    let containedBeacons =
        sensors
        |> Array.filter (fun s -> y = snd s.ClosestBeacon)
        |> Array.map (fun s -> fst s.ClosestBeacon) // x values
        |> Array.distinct
        |> Array.filter (fun x -> orderedRangeContains x ranges)
        |> Array.length

    let notBeacons = orderedRangeSize ranges - containedBeacons

    printfn "Positions not containing a beacon: %i" notBeacons

let part2() =
    let targetRange = { Min = 0; Max = 4000000 }
    let mutable foundBeacon = false
    let mutable x, y = 0, 0
    
    while not foundBeacon && y <= 4000000 do
        let ranges = getRanges y
        if orderedRangeContainsRange ranges targetRange then
            y <- y + 1
        else
            foundBeacon <- true
            x <-
                ranges.Ranges
                |> Array.find (fun r -> r.Max >= targetRange.Min)
                |> (fun r -> r.Max + 1)
    
    let frequency = (int64 x) * 4000000L + (int64 y)

    printfn "Frequency: %i" frequency
