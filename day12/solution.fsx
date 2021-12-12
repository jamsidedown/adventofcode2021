open System.IO
open System.Collections.Generic

type Cave =
    | Big of string
    | Small of string

let isUpper (input:string) =
    input.ToUpper() = input

let readInput (filepath:string) =
    File.ReadAllLines filepath
    |> Array.map (fun line -> line.Split '-')
    |> Array.map (fun line ->
        line
        |> Array.map (fun cave ->
            match cave with
            | c when isUpper c -> Big cave
            | _ -> Small cave))
    |> Array.map (fun line ->
        match line with
        | [| a; b |] -> Some (a, b)
        | _ -> None)
    |> Array.choose id

let mapPaths (input:array<Cave*Cave>) =
    input
    |> Array.toList
    |> List.collect (fun (a, b) -> [ (a, b); (b, a) ])
    |> List.groupBy fst
    |> List.map (fun (i, pairs) -> (i, pairs |> List.map snd))
    |> List.map (fun (i, caves) -> (i, caves |> List.filter (fun cave -> cave <> Small "start")))
    |> dict

let partOne (input:array<Cave*Cave>) =
    let paths = mapPaths input

    let rec recurse (current:Cave) (visited:Set<Cave>) : list<list<string>> =
        let next = paths[current] |> List.filter (fun c -> visited.Contains c |> not)
        match current with
        | Small "end" -> [ ["end"] ]
        | Small cave -> next |> List.collect (fun n -> recurse n (visited.Add current) |> List.map (fun f -> cave :: f))
        | Big cave -> next |> List.collect (fun n -> recurse n visited |> List.map (fun f -> cave :: f))

    recurse (Small "start") Set.empty |> List.length

let partTwo (input:array<Cave*Cave>) =
    let paths = mapPaths input

    let rec recurse (current:Cave) (visited:Set<Cave>) (twice:Option<Cave>) =
        let next =
            match twice with
            | Some _ -> paths[current] |> List.filter (fun c -> visited.Contains c |> not)
            | None -> paths[current]

        match current with
        | Small "end" -> [ ["end"] ]
        | Small cave ->
            match visited.Contains current with
            | false ->
                next |> List.collect (fun n -> recurse n (visited.Add current) twice |> List.map (fun f -> cave :: f))
            | true ->
                match twice with
                | Some _ -> []
                | None -> next |> List.collect (fun n -> recurse n visited (Some current) |> List.map (fun f -> cave :: f))
        | Big cave -> next |> List.collect (fun n -> recurse n visited twice |> List.map (fun f -> cave :: f))

    recurse (Small "start") Set.empty None |> List.length

let testInput1 = readInput "day12/testInput1.txt"
assert (partOne testInput1 = 10)
assert (partTwo testInput1 = 36)

let testInput2 = readInput "day12/testInput2.txt"
assert (partOne testInput2 = 19)
assert (partTwo testInput2 = 103)

let testInput3 = readInput "day12/testInput3.txt"
assert (partOne testInput3 = 226)
assert (partTwo testInput3 = 3509)

let input = readInput "day12/input.txt"
printfn $"{partOne input}"
printfn $"{partTwo input}"
