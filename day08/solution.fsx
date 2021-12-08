open System.IO

let readInput filepath =
    File.ReadAllLines filepath
    |> Array.map (fun line ->
        match line.Split " | " with
        | [| a; b |] -> Some (a.Split ' ', b.Split ' ')
        | _ -> None)
    |> Array.choose id

let partOne (input:array<array<string>*array<string>>) =
    input
    |> Array.map snd
    |> Array.sumBy (fun line ->
        let segmentCounts = Set.ofList [ 2; 3; 4; 7 ]
        line
        |> Array.filter (fun num ->
            match num.Length with
            | n when segmentCounts.Contains n -> true
            | _ -> false)
        |> Array.length)

let testInput = readInput "day08/testInput.txt"
assert (partOne testInput = 26)

let input = readInput "day08/input.txt"
printfn $"{partOne input}"
