module Day8

open System
open System.IO

let trees =
    File.ReadAllLines(@"input\day8.txt")
    |> Array.map (Seq.map (fun c -> int c - 48))
    |> array2D

let rows = Array2D.length1 trees
let cols = Array2D.length2 trees

let part1() =
    let visible = Array2D.create rows cols false

    for row in 0 .. (rows - 1) do
        let mutable highestFromLeft = -1
        for col in 0 .. (cols - 1) do
            if trees.[row, col] > highestFromLeft then
                visible.[row, col] <- true
                highestFromLeft <- trees.[row, col]

        let mutable highestFromRight = -1
        for col in (cols - 1) .. -1 .. 0 do
            if trees.[row, col] > highestFromRight then
                visible.[row, col] <- true
                highestFromRight <- trees.[row, col]

    for col in 0 .. (cols - 1) do
        let mutable highestFromTop = -1
        for row in 0 .. (rows - 1) do
            if trees.[row, col] > highestFromTop then
                visible.[row, col] <- true
                highestFromTop <- trees.[row, col]

        let mutable highestFromBottom = -1
        for row in (rows - 1) .. -1 .. 0 do
            if trees.[row, col] > highestFromBottom then
                visible.[row, col] <- true
                highestFromBottom <- trees.[row, col]

    let mutable totalVisible = 0

    for row in 0 .. (rows - 1) do
        for col in 0 .. (cols - 1) do
            if visible.[row, col] then totalVisible <- totalVisible + 1

    printfn "Total visible trees: %i" totalVisible

let part2() =
    let scenicScore row col =
        let viewingDist dRow dCol =
            if row = 0 || col = 0 || row = rows - 1 || col = cols - 1 then
                0
            else
                let mutable viewRow = row + dRow
                let mutable viewCol = col + dCol
                let mutable visible = 1

                while (viewRow >= 1
                       && viewRow < rows - 1
                       && viewCol >= 1
                       && viewCol < cols - 1
                       && trees.[viewRow, viewCol] < trees.[row, col]) do
                    visible <- visible + 1
                    viewRow <- viewRow + dRow
                    viewCol <- viewCol + dCol

                visible

        (viewingDist 0 1) * (viewingDist 0 -1) * (viewingDist 1 0) * (viewingDist -1 0)

    let scenicScores = Array2D.init rows cols scenicScore

    let mutable maxScore = 0

    for row in 0 .. (rows - 1) do
        for col in 0 .. (cols - 1) do
            if scenicScores.[row, col] > maxScore then maxScore <- scenicScores.[row, col]

    printfn "Maximum scenic score: %i" maxScore
