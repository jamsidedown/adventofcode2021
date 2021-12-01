open System.IO

let testInput = [| 199; 200; 208; 210; 200; 207; 240; 269; 260; 263; |]

let readInput (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map int

let countIncreases (depths:array<int>) =
    depths
    |> Array.pairwise
    |> Array.where(fun (x, y) -> y > x)
    |> Array.length

let partOne (depths:array<int>) =
    countIncreases depths

assert (partOne testInput = 7)

let partTwo (depths:array<int>) =
    depths
    |> Array.windowed 3
    |> Array.map Array.sum
    |> countIncreases

assert (partTwo testInput = 5)

let input = readInput "day01/input.txt"

printfn $"{partOne input}"
printfn $"{partTwo input}"
