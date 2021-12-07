open System
open System.IO

let testInput = [| 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 |]

let partOne (input:array<int>) =
    let min = input |> Array.min
    let max = input |> Array.max
    [| min..max |]
    |> Array.map (fun x ->
        let distance =
            input |> Array.sumBy (fun y -> Math.Abs(x - y))
        (x, distance))
    |> Array.minBy snd
    |> snd

let rec calcDistance (target:int) (jump:int) (crab:int) =
    match crab with
    | c when c < target -> jump + calcDistance target (jump + 1) (crab + 1)
    | c when c > target -> jump + calcDistance target (jump + 1) (crab - 1)
    | _ -> 0

let partTwo (input:array<int>) =
    let min = input |> Array.min
    let max = input |> Array.max
    [| min..max |]
    |> Array.map (fun x ->
        let distance = input |> Array.sumBy (calcDistance x 1)
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