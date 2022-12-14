module Day14

open System
open System.IO

type Position = int * int
type RockPath = Position []

let parsePosition (s: string) : Position =
    let coords = s.Split(',')
    coords.[0] |> int, coords.[1] |> int

let parseLine (s: string) : RockPath =
    s.Split(" -> ")
    |> Array.map parsePosition

let rockpaths =
    File.ReadAllLines(@"input\day14.txt")
    |> Array.map parseLine

let allPositions = Array.concat rockpaths

let minY = allPositions |> Array.map snd |> Array.min |> min 0
let maxY = (allPositions |> Array.map snd |> Array.max) + 2

let maxPossibleAcrossFromStart = maxY - minY

let minX = (allPositions |> Array.map fst |> Array.min) - 1 |> min (500 - maxPossibleAcrossFromStart)
let maxX = (allPositions |> Array.map fst |> Array.max) + 1 |> max (500 + maxPossibleAcrossFromStart)


let rockGrid =
    let arr = Array2D.createBased minX minY (1 + maxX - minX) (1 + maxY - minY) false

    rockpaths
    |> Array.iter (fun rp ->
        rp
        |> Array.windowed 2
        |> Array.iter (fun [| (x1, y1); (x2, y2) |] ->
            let dx = sign (x2 - x1)
            let dy = sign (y2 - y1)
            let mutable x, y = x1, y1
            
            while x <> x2 || y <> y2 do
                arr.[x, y] <- true
                x <- x + dx
                y <- y + dy
            arr.[x2, y2] <- true))
    arr

let part1() =
    //for y in minY .. maxY do
    //    for x in minX .. maxX do
    //        printf "%s" (if rockGrid.[x, y] then "#" else ".")
    //    printfn ""

    let grid = Array2D.copy rockGrid
    
    let mutable sandAtRest = 0
    let mutable endlessVoid = false

    while not endlessVoid do
        let mutable x, y = 500, 0
        let mutable sandMoving = true

        while sandMoving do
            if y = maxY then
                sandMoving <- false
                endlessVoid <- true
            elif not grid.[x, y + 1] then
                y <- y + 1
            elif not grid.[x - 1, y + 1] then
                x <- x - 1
                y <- y + 1
            elif not grid.[x + 1, y + 1] then
                x <- x + 1
                y <- y + 1
            else
                sandMoving <- false
                sandAtRest <- sandAtRest + 1
                grid.[x, y] <- true

    //for y in minY .. maxY do
    //    for x in minX .. maxX do
    //        printf "%s" (if grid.[x, y] then "#" else ".")
    //    printfn ""

    printfn "Sand at rest: %i" sandAtRest

let part2() =
    let grid = Array2D.copy rockGrid

    for x in minX .. maxX do
        grid.[x, maxY] <- true
    
    let mutable sandAtRest = 0
    let mutable startBlocked = false

    while not startBlocked do
        let mutable x, y = 500, 0
        let mutable sandMoving = true

        while sandMoving do
            if y = maxY then
                failwith "error"
            elif not grid.[x, y + 1] then
                y <- y + 1
            elif not grid.[x - 1, y + 1] then
                x <- x - 1
                y <- y + 1
            elif not grid.[x + 1, y + 1] then
                x <- x + 1
                y <- y + 1
            else
                sandMoving <- false
                sandAtRest <- sandAtRest + 1
                grid.[x, y] <- true
                if x = 500 && y = 0 then startBlocked <- true

    //for y in minY .. maxY do
    //    for x in minX .. maxX do
    //        printf "%s" (if grid.[x, y] then "#" else ".")
    //    printfn ""

    printfn "Sand at rest: %i" sandAtRest
