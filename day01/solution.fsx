open System.IO

let testInput = [| 199; 200; 208; 210; 200; 207; 240; 269; 260; 263; |]

let readInput filepath =
    File.ReadAllLines filepath
    |> Array.map int

let partOne (depths:array<int>) =
    Array.zip depths[1..] depths[..depths.Length-2]
    |> Array.sumBy (fun (u, l) ->
        match u, l with
        | u, l when u > l -> 1
        | _ -> 0)

assert (partOne testInput = 7)

let input = readInput "day01/input.txt"

printfn $"{partOne input}"
