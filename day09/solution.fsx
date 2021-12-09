open System.IO

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

let lowPoints (points:array<array<int>>) =
    neighbours points
    |> Seq.filter (fun pair ->
        let (x, y) = pair.Key
        let p = points[x][y]
        pair.Value
        |> Array.forall (fun (xx, yy) -> points[xx][yy] > p))

let partOne (points:array<array<int>>) =
    lowPoints points
    |> Seq.sumBy (fun pair ->
        let (x, y) = pair.Key
        points[x][y] + 1)

let basin (points:array<array<int>>) (target:int*int) =
    let pointNeighbours = neighbours points
    let mutable exclude = Set.ofList [ target ]

    let rec recurse (current:int*int) : list<int*int> =
        let currentNeighbours =
            pointNeighbours[current]
            |> Array.filter (fun c -> exclude.Contains c |> not)
            |> Array.filter (fun (x, y) -> points[x][y] < 9)
            |> Array.toList

        exclude <- currentNeighbours |> Set.ofList |> Set.union exclude

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
