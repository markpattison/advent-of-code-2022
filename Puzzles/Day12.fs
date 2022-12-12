module Day12

open System
open System.IO

let toHeights (s: string) =
    s.ToCharArray()
    |> Array.map (fun c -> int c - 97)

let heightMap, (xStart, yStart), (xEnd, yEnd) =
    let rawHeights =
        File.ReadAllLines(@"input\day12.txt")
        |> array2D

    let mutable start = 0, 0
    let mutable end' = 0, 0

    let heights =
        rawHeights
        |> Array2D.mapi (fun x y ch ->
            match ch with
            | 'S' ->
                start <- x, y
                0
            | 'E' ->
                end' <- x, y
                25
            | c -> int c - 97)

    heights, start, end'

let part1() =
    let width = heightMap.GetLength(0)
    let height = heightMap.GetLength(1)

    let shortestPathTo = Array2D.create width height -1
    shortestPathTo.[xStart, yStart] <- 0

    let mutable foundEnd = false
    let mutable stepsSoFar = 0

    while not foundEnd do
        for x in 0 .. width - 1 do
            for y in 0 .. height - 1 do
                let h = heightMap.[x, y]

                if x = xEnd && y = yEnd && shortestPathTo.[x, y] > -1 then foundEnd <- true

                if shortestPathTo.[x, y] = stepsSoFar then
                    if x > 0 && shortestPathTo.[x - 1, y] = -1 && (heightMap.[x - 1, y] - h) < 2 then shortestPathTo.[x - 1, y] <- stepsSoFar + 1
                    if x < width - 1 && shortestPathTo.[x + 1, y] = -1 && (heightMap.[x + 1, y] - h) < 2 then shortestPathTo.[x + 1, y] <- stepsSoFar + 1
                    if y > 0 && shortestPathTo.[x, y - 1] = -1 && (heightMap.[x, y - 1] - h) < 2 then shortestPathTo.[x, y - 1] <- stepsSoFar + 1
                    if y < height - 1 && shortestPathTo.[x, y + 1] = -1 && (heightMap.[x, y + 1] - h) < 2 then shortestPathTo.[x, y + 1] <- stepsSoFar + 1

        stepsSoFar <- stepsSoFar + 1

    printfn "Shortest path: %i " shortestPathTo.[xEnd, yEnd]

let part2() =
    let width = heightMap.GetLength(0)
    let height = heightMap.GetLength(1)

    let shortestPathTo = Array2D.create width height -1
    shortestPathTo.[xEnd, yEnd] <- 0

    let mutable foundBase = false
    let mutable shortest = Int32.MaxValue
    let mutable stepsSoFar = 0

    while not foundBase do
        for x in 0 .. width - 1 do
            for y in 0 .. height - 1 do
                let h = heightMap.[x, y]

                if h = 0 && shortestPathTo.[x, y] > -1  then
                    foundBase <- true
                    shortest <- min shortestPathTo.[x, y] shortest

                if shortestPathTo.[x, y] = stepsSoFar then
                    if x > 0 && shortestPathTo.[x - 1, y] = -1 && (heightMap.[x - 1, y] - h) > -2 then shortestPathTo.[x - 1, y] <- stepsSoFar + 1
                    if x < width - 1 && shortestPathTo.[x + 1, y] = -1 && (heightMap.[x + 1, y] - h) > -2 then shortestPathTo.[x + 1, y] <- stepsSoFar + 1
                    if y > 0 && shortestPathTo.[x, y - 1] = -1 && (heightMap.[x, y - 1] - h) > -2 then shortestPathTo.[x, y - 1] <- stepsSoFar + 1
                    if y < height - 1 && shortestPathTo.[x, y + 1] = -1 && (heightMap.[x, y + 1] - h) > -2 then shortestPathTo.[x, y + 1] <- stepsSoFar + 1

        stepsSoFar <- stepsSoFar + 1

    printfn "Shortest path: %i " shortest
