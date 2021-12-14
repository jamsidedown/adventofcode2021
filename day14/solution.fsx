open System.IO
open System.Collections.Generic

let readInput (filepath:string) =
    let lines = File.ReadAllLines filepath
    let start = lines[0] |> Seq.toList |> List.map string
    let rules =
        lines
        |> Array.skip 2
        |> Array.map (fun line ->
            match line.Split " -> " with
            | [| a; b |] -> Some (a, b)
            | _ -> None)
        |> Array.choose id
        |> dict
    (start, rules)

let partOne (current:list<string>) (rules:IDictionary<string, string>) =
    let rec recurse (current:list<string>) (rules:IDictionary<string, string>) =
        match current with
        | a :: b :: cs -> a :: rules[a + b] :: recurse (b :: cs) rules
        | [ b ] -> [ b ]
        | _ -> []

    let mutable current = current
    for _ in [ 1..10 ] do
        current <- recurse current rules

    let counts = current |> List.countBy id
    let mostCommon = counts |> List.sortByDescending snd |> List.head
    let leastCommon = counts |> List.sortBy snd |> List.head
    (snd mostCommon) - (snd leastCommon)

let add (first:IDictionary<string, int64>) (second:IDictionary<string, int64>) : IDictionary<string, int64> =
    let output = new Dictionary<string, int64>()

    for key in first.Keys do
        output[key] <- first[key]

    for key in second.Keys do
        match output.ContainsKey key with
        | true -> output[key] <- output[key] + second[key]
        | false -> output[key] <- second[key]

    output

let partTwo (current:list<string>) (rules:IDictionary<string, string>) =
    let mutable cache = new Dictionary<string*string*int, IDictionary<string, int64>>()
    let rec recurse (first:string) (second:string) (steps:int) =
        let key = (first, second, steps)
        match cache.ContainsKey key with
        | true -> cache[key]
        | false ->
            match steps with
            | 0 -> [ (second, 1L) ] |> dict
            | n ->
                let middle = rules[first + second]
                let result = add (recurse first middle (n - 1)) (recurse middle second (n - 1))
                cache[key] <- result
                result

    let counts =
        current
        |> List.pairwise
        |> List.map (fun (a, b) -> recurse a b 40)
        |> List.reduce add
        |> add ([current.Head, 1L] |> dict)
        |> Seq.map (fun pair -> (pair.Key, pair.Value))
        |> Seq.toArray

    let max = counts |> Array.sortByDescending snd |> Array.head
    let min = counts |> Array.sortBy snd |> Array.head

    (snd max) - (snd min)


let (testStart, testRules) = readInput "day14/testInput.txt"
assert (partOne testStart testRules = 1588)
assert (partTwo testStart testRules = 2188189693529L)

let (start, rules) = readInput "day14/input.txt"
printfn $"{partOne start rules}"
printfn $"{partTwo start rules}"


