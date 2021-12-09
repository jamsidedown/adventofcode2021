open System.IO

type PointType =
    | Point of int
    | Lowpoint of int

let readInput (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map (fun line ->
        line
        |> Seq.toArray
        |> Array.map (string >> int))

let partOne (input:array<array<int>>) =
    let mutable points = input |> Array.map (fun row -> row |> Array.map Point)

    let height = points.Length
    let width = points[0].Length

    for x in [| 0..(height-1) |] do
        for y in [| 0..(width-1) |] do
            let current =
                match points[x][y] with
                | Point n -> n
                | Lowpoint n -> n
            [| (x-1, y); (x+1, y); (x, y-1); (x, y+1) |]
            |> Array.filter (fun (x, y) -> x >= 0 && y >= 0)
            |> Array.filter (fun (x, y) -> x < height && y < width)
            |> Array.map (fun (x, y) -> points[x][y])
            |> Array.map (fun point ->
                match point with
                | Point n -> n
                | Lowpoint n -> n)
            |> Array.filter (fun n -> n <= current)
            |> function
                | [||] -> points[x][y] <- Lowpoint current
                | _ -> ()

    points
    |> Array.sumBy (fun row ->
        row
        |> Array.sumBy (fun cell ->
            match cell with
            | Lowpoint n -> n + 1
            | _ -> 0))

let testInput = readInput "day09/testInput.txt"
assert (partOne testInput = 15)

let input = readInput "day09/input.txt"
printfn $"{partOne input}"
