open System.IO
open System.Collections.Generic

let readInput (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map (Seq.toArray >> Array.map (string >> int))

let getNeighbours (grid:array<array<int>>) (x:int) (y:int) =
    [| (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) |]
    |> Array.filter (fun (x, _) -> x >= 0 && x < (grid.Length))
    |> Array.filter (fun (_, y) -> y >= 0 && y < grid[0].Length)

let partOne (grid:array<array<int>>) =
    let mutable paths = new Dictionary<int*int, list<int*int>*int>()
    let rec recurse (coord:int*int) (path:list<int*int>) (risk:int) =
        let (x, y) = coord
        let coordRisk = risk + grid[x][y]
        let coordPath = coord :: path
        match paths.ContainsKey coord with
        | false ->
            paths[coord] <- (coordPath, coordRisk)
            for neighbour in getNeighbours grid x y do
                recurse neighbour coordPath coordRisk
        | true ->
            match paths[coord] with
            | _, r when r > coordRisk ->
                paths[coord] <- (coordPath, coordRisk)
                for neighbour in getNeighbours grid x y do
                    recurse neighbour coordPath coordRisk
            | _ -> ()

    recurse (0, 0) [] 0

    let target = (grid.Length - 1, grid[0].Length - 1)
    match paths.ContainsKey target with
    | false -> 0
    | true ->
        let (path, risk) = paths[target]
        path |> List.rev |> printfn "%A"
        path |> List.rev |> List.map (fun (x, y) -> grid[x][y]) |> printfn "%A"
        risk - grid[0][0]

let testInput = readInput "day15/testInput.txt"
assert (partOne testInput = 40)

let input = readInput "day15/input.txt"
printfn $"{partOne input}"
