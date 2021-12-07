open System
open System.IO

let testInput = [| 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 |]

let minMax (input:array<int>) =
    [| (input |> Array.min)..(input |> Array.max) |]

let partOne (input:array<int>) =
    input
    |> minMax
    |> Array.map (fun x ->
        let distance =
            input |> Array.sumBy (fun y -> Math.Abs(x - y))
        (x, distance))
    |> Array.minBy snd
    |> snd

let calcDistance (target:int) (crab:int) =
    let n = Math.Abs(target - crab)
    (n * (n + 1)) / 2

let partTwo (input:array<int>) =
    input
    |> minMax
    |> Array.map (fun x ->
        let distance = input |> Array.sumBy (calcDistance x)
        (x, distance))
    |> Array.minBy snd
    |> snd

assert (partOne testInput = 37)
assert (partTwo testInput = 168)

let input =
    (File.ReadAllText "day07/input.txt").Split ','
    |> Array.map int

printfn $"{partOne input}"
printfn $"{partTwo input}"