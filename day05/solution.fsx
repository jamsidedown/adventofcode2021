open System
open System.IO

type Coord = { X:int ; Y:int }
type Line = { Start:Coord; Stop:Coord }
type GridPosition =
    | Uncrossed
    | Crossed of int

let readInput filepath =
    File.ReadAllLines filepath
    |> Array.map (fun line ->
        match  line.Split([| " -> "; "," |], StringSplitOptions.None) with
        | [| x1; y1; x2; y2 |] ->
            Some { Start={ X=(int x1); Y=(int y1) }; Stop={ X=(int x2); Y=(int y2) } }
        | _ -> None)
    |> Array.choose id

let isHorizontal (line:Line) =
    line.Start.Y = line.Stop.Y

let isVertical (line:Line) =
    line.Start.X = line.Stop.X

let rec limits (lines:list<Line>) =
    match lines with
    | l :: ls ->
        let lx = Math.Max(l.Start.X, l.Stop.X)
        let ly = Math.Max(l.Start.Y, l.Stop.Y)
        let nl = limits ls
        { nl with
            X=(Math.Max(lx, nl.X));
            Y=(Math.Max(ly, nl.Y))
        }
    | _ -> { X=0; Y=0 }

let range (start:int) (stop:int) =
    match start with
    | start when start <= stop -> [| start..stop |]
    | _ ->
        let r = [| stop..start |]
        Array.Reverse r
        r

let expand (line:Line) =
    let xs = range line.Start.X line.Stop.X
    let ys = range line.Start.Y line.Stop.Y

    match (xs, ys) with
    | (xs, ys) when xs.Length = ys.Length ->
        Array.zip xs ys
        |> Array.map (fun (x, y) -> { X=x; Y=y })
    | (xs, ys) when xs.Length > ys.Length ->
        let y = Array.head ys
        xs |> Array.map (fun x -> { X=x; Y=y })
    | (xs, ys) when xs.Length < ys.Length ->
        let x = Array.head xs
        ys |> Array.map (fun y -> { X=x; Y=y })
    | _ -> [||]

let generateGrid (input:array<Line>) =
    input
    |> Array.toList
    |> limits
    |> fun max ->
        [| 0..max.X |]
        |> Array.map (fun _ ->
            [| 0..max.Y |]
            |> Array.map (fun _ -> Uncrossed))

let partOne (input:array<Line>) =
    let filteredInput =
        input
        |> Array.filter (fun line ->
            isVertical line || isHorizontal line)

    let mutable grid = generateGrid filteredInput

    for line in filteredInput do
        for coord in expand line do
            match grid[coord.X][coord.Y] with
            | Uncrossed -> grid[coord.X][coord.Y] <- Crossed 1
            | Crossed n -> grid[coord.X][coord.Y] <- Crossed (n + 1)

    grid
    |> Array.collect id
    |> Array.filter (fun position ->
        match position with
        | Crossed n when n >= 2 -> true
        | _ -> false)
    |> Array.length

let partTwo (input:array<Line>) =
    let mutable grid = generateGrid input

    for line in input do
        for coord in expand line do
            match grid[coord.X][coord.Y] with
            | Uncrossed -> grid[coord.X][coord.Y] <- Crossed 1
            | Crossed n -> grid[coord.X][coord.Y] <- Crossed (n + 1)

    grid
    |> Array.collect id
    |> Array.filter (fun position ->
        match position with
        | Crossed n when n >= 2 -> true
        | _ -> false)
    |> Array.length

let testInput = readInput "day05/testInput.txt"
assert (partOne testInput = 5)
assert (partTwo testInput = 12)

let input = readInput "day05/input.txt"
printfn $"{partOne input}"
printfn $"{partTwo input}"
