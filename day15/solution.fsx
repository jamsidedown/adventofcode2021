open System.IO
open System.Collections.Generic

let readInput (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map (Seq.toArray >> Array.map (string >> int))

let getNeighbours (grid:array<array<int>>) (x:int) (y:int) =
    [| (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) |]
    |> Array.filter (fun (x, _) -> x >= 0 && x < (grid.Length))
    |> Array.filter (fun (_, y) -> y >= 0 && y < grid[0].Length)

let getAllNeighbours (grid:array<array<int>>) =
    [| 0..(grid.Length-1) |]
    |> Array.collect (fun x ->
        [| 0..(grid[0].Length-1) |]
        |> Array.map (fun y ->
            ((x, y), getNeighbours grid x y)))
    |> dict

let partOne (grid:array<array<int>>) =
    let mutable shortest = new Dictionary<int*int, int>()
    let allNeighbours = getAllNeighbours grid

    let rec initialise (coord:int*int) (risk:int) =
        let (x, y) = coord
        let coordRisk = risk + grid[x][y]
        match shortest.TryGetValue coord with
        | (false, _) ->
            shortest[coord] <- coordRisk
            initialiseNeighbours x y coordRisk
        | (true, value) when value > coordRisk ->
            shortest[coord] <- coordRisk
            initialiseNeighbours x y coordRisk
        | _ -> ()
    and initialiseNeighbours (x:int) (y:int) (risk:int) =
        match x + 1 with
        | xx when xx < grid.Length -> initialise (xx, y) risk
        | _ -> ()
        match y + 1 with
        | yy when yy < grid[0].Length -> initialise (x, yy) risk
        | _ -> ()

    initialise (0, 0) 0

    let rec recurse (coord:int*int) (risk:int) =
        let neighbours = allNeighbours[coord]
        for neighbour in neighbours do
            let (nx, ny) = neighbour
            let newRisk = risk + grid[nx][ny]
            if newRisk < shortest[neighbour] then
                shortest[neighbour] <- newRisk
                recurse neighbour newRisk
    and checkNeighbours (coord:int*int) =
        let (x, y) = coord
        let coordRisk = grid[x][y]
        let lowestNeighbour =
            allNeighbours[coord]
            |> Array.sortBy (fun n -> shortest[n])
            |> Array.head
        let newRisk = shortest[lowestNeighbour] + coordRisk
        if newRisk < shortest[coord] then
            shortest[coord] <- newRisk
            recurse coord newRisk

    for coord in shortest.Keys do
        checkNeighbours coord

    let target = (grid.Length - 1, grid[0].Length - 1)
    shortest[target] - grid[0][0]

let resize (grid:array<array<int>>) (factor:int) =
    let rec correct (x:int) (m:int) =
        match x >= m with
        | true -> correct ((x % m) + 1) m
        | false -> x

    grid
    |> Array.map (fun row ->
        Array.zeroCreate (row.Length * factor)
        |> Array.mapi (fun i _ -> correct (row[i % row.Length] + (i / row.Length)) 10))
    |> fun rows ->
        Array.zeroCreate (rows.Length * factor)
        |> Array.mapi (fun i _ ->
            rows[i % rows.Length]
            |> Array.map (fun x ->
                correct (x + (i / rows.Length)) 10))

let pprint (grid:array<array<int>>) =
    let gs = grid |> Array.map (fun row -> row |> Array.map string |> String.concat "")
    for row in gs do
        printfn "%s" row

let partTwo (grid:array<array<int>>) =
    resize grid 5 |> partOne

let testInput = readInput "day15/testInput.txt"
assert (partOne testInput = 40)
assert (partTwo testInput = 315)

let input = readInput "day15/input.txt"
printfn $"{partOne input}"
printfn $"{partTwo input}"
