open System.IO

let testInput = [| 199; 200; 208; 210; 200; 207; 240; 269; 260; 263; |]

let readInput (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map int

let countIncreases (depths:array<int>) =
    depths
    |> Array.windowed 2
    |> Array.sumBy (fun arr ->
        match arr with
        | [| x; y |] when y > x -> 1
        | _ -> 0)

let partOne (depths:array<int>) =
    depths
    |> countIncreases

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
