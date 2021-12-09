open System.IO
open System.Collections.Generic

let readInput (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map (fun line ->
        line
        |> Seq.toArray
        |> Array.map (string >> int))

let neighbours (points:array<array<int>>) =
    let height = points.Length
    let width = points[0].Length

    [| 0..(height-1) |]
    |> Array.collect (fun x ->
        [| 0..(width-1) |]
        |> Array.map (fun y ->
            [| (x-1, y); (x+1, y); (x, y-1); (x, y+1) |]
            |> Array.filter (fun (x, y) -> x >= 0 && y >= 0)
            |> Array.filter (fun (x, y) -> x < height && y < width)
            |> fun arr -> ((x, y), arr)))
    |> dict

let isLowest (points:array<array<int>>) (current:int*int) (ns:array<int*int>) =
    ns
    |> Array.filter (fun (x, y) -> points[x][y] <= points[fst current][snd current])
    |> Array.length = 0

let lowPoints (points:array<array<int>>) =
    neighbours points
    |> Seq.filter (fun pair -> isLowest points pair.Key pair.Value)

let partOne (points:array<array<int>>) =
    lowPoints points
    |> Seq.sumBy (fun pair ->
        let (x, y) = pair.Key
        points[x][y] + 1)

let basin (points:array<array<int>>) (target:int*int) =
    let ns = neighbours points
    let mutable basin = Set.ofList [ target ]

    let includes (coords:array<int*int>) =
        coords
        |> Array.filter (fun coord -> not (basin.Contains coord))
        |> Array.filter (fun (x, y) -> points[x][y] < 9)
        |> Array.toList

    let rec recurse (current:int*int) : list<int*int> =
        let point = points[fst current][snd current]
        let currentNeighbours = includes ns[current] |> List.filter (fun (x, y) -> points[x][y] > point)
        basin <- Set.union basin (Set.ofList currentNeighbours)
        currentNeighbours |> List.collect (fun n -> n :: recurse n)

    target :: recurse target

let partTwo (points:array<array<int>>) =
    lowPoints points
    |> Seq.map (fun pair -> basin points pair.Key)
    |> Seq.map List.length
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (*)

let testInput = readInput "day09/testInput.txt"
assert (partOne testInput = 15)
assert (partTwo testInput = 1134)

let input = readInput "day09/input.txt"
printfn $"{partOne input}"
printfn $"{partTwo input}"
