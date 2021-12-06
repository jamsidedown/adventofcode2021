open System
open System.Collections.Generic
open System.IO

let testInput = [| 3; 4; 3; 1; 2 |]

let run (days:int) (input:array<int>) =
    let cache = new Dictionary<int*int, int64>()

    let rec inner (days:int) (fish:int) =
        let key = (days, fish)
        match days with
        | 0 -> 1L
        | _ -> 
            match cache.ContainsKey key with
            | true -> cache[key]
            | false ->
                let next =
                    match fish with
                    | 0 -> (inner (days - 1) 6) + (inner (days - 1) 8)
                    | n -> inner (days - 1) (n - 1)
                cache[key] <- next
                next

    input |> Array.sumBy (inner days)

let partOne (input:array<int>) =
    run 80 input

let partTwo (input:array<int>) =
    run 256 input

assert (run 18 testInput = 26)
assert (partOne testInput = 5934)
assert (partTwo testInput = 26984457539L)

let input =
    File.ReadAllText "day06/input.txt"
    |> (fun s -> s.Split(',', StringSplitOptions.RemoveEmptyEntries))
    |> Array.map int

printfn $"{partOne input}"
printfn $"{partTwo input}"