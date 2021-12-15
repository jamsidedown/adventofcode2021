open System
open System.IO

type Coord = { x : int; y : int; }
type Path = { distance: int; previous: Coord; }

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

let resize (factor:int) (grid:array<array<int>>) =
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

let pathGrid (width:int) (height:int) =
    let mutable grid =
        [| 0..width-1 |]
        |> Array.map (fun x ->
            [| 0..height-1 |]
            |> Array.map (fun y ->
                { distance = Int32.MaxValue; previous = { x = x; y = y } }))
    grid[0][0] <- { grid[0][0] with distance = 0 }
    grid

let dijkstra (grid:array<array<int>>) =
    let paths = pathGrid grid.Length grid[0].Length
    let allNeighbours = getAllNeighbours grid
    let mutable active = Set.ofList [ (0, 0) ]

    while not active.IsEmpty do
        let source = active |> Seq.sortBy (fun (x, y) -> (paths[x][y]).distance) |> Seq.head
        let (x, y) = source
        let distance = (paths[x][y]).distance
        let neighbours = allNeighbours[source] |> Seq.filter (active.Contains >> not)
        for neighbour in neighbours do
            let (nx, ny) = neighbour
            let neighbourDistance = distance + grid[nx][ny]
            if neighbourDistance < (paths[nx][ny]).distance then
                paths[nx][ny] <- { distance = neighbourDistance; previous = { x = x; y = y } }
                active <- active.Add neighbour
        active <- active.Remove source

    paths

let partOne (grid:array<array<int>>) =
    let paths = dijkstra grid
    (paths[grid.Length - 1][grid[0].Length - 1]).distance

let partTwo (grid:array<array<int>>) =
    grid |> resize 5 |> partOne

let testInput = readInput "day15/testInput.txt"
assert (partOne testInput = 40)
assert (partTwo testInput = 315)

let input = readInput "day15/input.txt"
printfn $"{partOne input}"
printfn $"{partTwo input}"
