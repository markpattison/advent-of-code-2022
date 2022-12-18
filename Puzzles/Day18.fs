module Day18

open System
open System.IO

let toCoords (s: string) =
    let arr = s.Split(',')
    (int arr.[0], int arr.[1], int arr.[2])

let allCoords =
    File.ReadAllLines(@"input\day18.txt")
    |> Array.map toCoords

let part1() =
    let countExposed (x, y, z) =
        let isClear co =
            if Array.contains co allCoords then 0 else 1

        isClear (x + 1, y, z)
        + isClear (x - 1, y, z)
        + isClear (x, y + 1, z)
        + isClear (x, y - 1, z)
        + isClear (x, y, z + 1)
        + isClear (x, y, z - 1)

    let surfaceArea =
        allCoords
        |> Array.sumBy countExposed

    printfn "Surface area: %i" surfaceArea

let part2() =
    let minX = -1 + (allCoords |> Array.map (fun (x, y, z) -> x) |> Array.min)
    let maxX =  1 + (allCoords |> Array.map (fun (x, y, z) -> x) |> Array.max)
    let minY = -1 + (allCoords |> Array.map (fun (x, y, z) -> y) |> Array.min)
    let maxY =  1 + (allCoords |> Array.map (fun (x, y, z) -> y) |> Array.max)
    let minZ = -1 + (allCoords |> Array.map (fun (x, y, z) -> z) |> Array.min)
    let maxZ =  1 + (allCoords |> Array.map (fun (x, y, z) -> z) |> Array.max)
    
    let sizeX = (maxX - minX) + 1
    let sizeY = (maxY - minY) + 1
    let sizeZ = (maxZ - minZ) + 1

    let external =
        Array3D.init sizeX sizeY sizeZ
            (fun x y z -> x = 0 || y = 0 || z = 0 || x = sizeX - 1 || y = sizeY - 1 || z = sizeZ - 1)

    let mutable finished = false

    let isClear (x, y, z) =
        Array.contains (x + minX, y + minY, z + minZ) allCoords |> not

    while not finished do
        let mutable foundExternal = false

        for x in 0 .. sizeX - 1 do
            for y in 0 .. sizeY - 1 do
                for z in 0 .. sizeZ - 1 do
                    if external.[x, y, z] then
                        if x < sizeX - 1 && not external.[x + 1, y, z] && isClear (x + 1, y, z) then
                            external.[x + 1, y, z] <- true
                            foundExternal <- true
                        if x > 0 && not external.[x - 1, y, z] && isClear (x - 1, y, z) then
                            external.[x - 1, y, z] <- true
                            foundExternal <- true
                        if y < sizeY - 1 && not external.[x, y + 1, z] && isClear (x, y + 1, z) then
                            external.[x, y + 1, z] <- true
                            foundExternal <- true
                        if y > 0 && not external.[x, y - 1, z] && isClear (x, y - 1, z) then
                            external.[x, y - 1, z] <- true
                            foundExternal <- true
                        if z < sizeZ - 1 && not external.[x, y, z + 1] && isClear (x, y, z + 1) then
                            external.[x, y, z + 1] <- true
                            foundExternal <- true
                        if z > 0 && not external.[x, y, z - 1] && isClear (x, y, z - 1) then
                            external.[x, y, z - 1] <- true
                            foundExternal <- true
        
        if not foundExternal then finished <- true

    let countExposed (x, y, z) =
        let isClearAndExternal (x', y', z') =
            if Array.contains (x', y', z') allCoords || not external.[x' - minX, y' - minY, z' - minZ] then 0 else 1

        isClearAndExternal (x + 1, y, z)
        + isClearAndExternal (x - 1, y, z)
        + isClearAndExternal (x, y + 1, z)
        + isClearAndExternal (x, y - 1, z)
        + isClearAndExternal (x, y, z + 1)
        + isClearAndExternal (x, y, z - 1)

    let externalSurfaceArea =
        allCoords
        |> Array.sumBy countExposed

    printfn "External surface area: %i" externalSurfaceArea
