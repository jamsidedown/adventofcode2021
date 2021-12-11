open System
open System.IO

let readInput (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map (Seq.toArray >> Array.map (string >> int))

let getNeighboursForOct (octs:array<array<int>>) (x:int) (y:int) =
    let dimX = octs.Length
    let dimY = octs[0].Length

    [| Math.Max(0, x-1)..(Math.Min(dimX-1, x+1)) |]
    |> Array.collect (fun xx ->
        [| Math.Max(0, y-1)..Math.Min(dimY-1, y+1) |]
        |> Array.map (fun yy -> (xx, yy)))
    |> Array.filter (fun (xx, yy) -> (xx, yy) <> (x, y))

let octNeighbours (octs:array<array<int>>) =
    let dimX = octs.Length
    let dimY = octs[0].Length

    [| 0..dimX-1 |]
    |> Array.collect (fun x ->
        [| 0..dimY-1 |]
        |> Array.map (fun y ->
            ((x, y), getNeighboursForOct octs x y)))
    |> dict

let copy (input:array<array<int>>) =
    input |> Array.map (Array.map id)

let step (input:array<array<int>>) =
    let mutable octs = copy input
    let mutable flashed = Set.empty

    let neighbours = octNeighbours octs

    let dimX = octs.Length
    let dimY = octs[0].Length

    for x in [| 0..dimX-1 |] do
        for y in [| 0..dimY-1 |] do
            octs[x][y] <- octs[x][y] + 1

    let rec flash (x:int) (y:int) =
        match (x, y) with
        | (x, y) when octs[x][y] > 9 ->
            octs[x][y] <- 0
            flashed <- flashed.Add (x, y)
            let ns = neighbours[(x, y)]
            for (nx, ny) in ns do
                match flashed.Contains((nx, ny)) with
                | true -> ()
                | false ->
                    octs[nx][ny] <- octs[nx][ny] + 1
                    flash nx ny
        | _ -> ()

    for x in [| 0..dimX-1 |] do
        for y in [| 0..dimY-1 |] do
            flash x y

    (octs, flashed |> Set.count)

let partOne (input:array<array<int>>) (steps:int) =
    let mutable octs = input
    [| 1..steps |]
    |> Array.sumBy (fun _ ->
        let (newOcts, flashed) = step octs
        octs <- newOcts
        flashed)

let partTwo (input:array<array<int>>) =
    let mutable octs = input
    let size = octs.Length * octs[0].Length

    let rec recurse () =
        match step octs with
        | _, s when s = size -> 1
        | newOcts, _ ->
            octs <- newOcts
            1 + recurse()

    recurse()

let testInput = readInput "day11/testInput.txt"
assert (partOne testInput 10 = 204)
assert (partOne testInput 100 = 1656)
assert (partTwo testInput = 195)

let input = readInput "day11/input.txt"
printfn $"{partOne input 100}"
printfn $"{partTwo input}"