open System
open System.Collections.Generic
open System.IO

let testInput = [| 3; 4; 3; 1; 2 |]

let partOne (days:int) (input:array<int>) =
    let rec inner (days:int) (input:array<int>) =
        match days with
        | 0 -> input
        | n ->
            input
            |> Array.collect (fun elem ->
                match elem with
                | 0 -> [| 6; 8 |]
                | x -> [| x - 1 |])
            |> inner (n - 1)
    inner days input
    |> Array.length

let partTwo (days:int) (input:array<int>) =
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

    input
    |> Array.sumBy (inner days)

assert (partOne 18 testInput = 26)
assert (partOne 80 testInput = 5934)

assert (partTwo 256 testInput = 26984457539L)

let input =
    File.ReadAllText "day06/input.txt"
    |> (fun s -> s.Split(',', StringSplitOptions.RemoveEmptyEntries))
    |> Array.map int

printfn $"{partOne 80 input}"
printfn $"{partTwo 256 input}"