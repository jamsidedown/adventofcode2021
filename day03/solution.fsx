open System
open System.IO

let testInput = [| "00100"; "11110"; "10110"; "10111"; "10101"; "01111"; "00111"; "11100"; "10000"; "11001"; "00010"; "01010"; |]

let fromBinary value =
    Convert.ToInt32(value, 2)

let countChars (input:array<string>) =
    input
    |> Seq.collect (fun line ->
        line
        |> Seq.mapi (fun i c -> (i, c)))
    |> Seq.groupBy fst
    |> Seq.map (fun (i, group) ->
        group
        |> Seq.map snd
        |> Seq.countBy id
        |> Seq.sortByDescending snd)

let mostCommon (input:seq<seq<char*int>>) =
    input
    |> Seq.map (fun line ->
        match Seq.toArray line with
        | [| (c, _); _ |] -> Some c
        | _ -> None)
    |> Seq.choose id

let leastCommon (input:seq<seq<char*int>>) =
    input
    |> Seq.map (fun line ->
        match Seq.toArray line with
        | [| _; (c, _) |] -> Some c
        | _ -> None)
    |> Seq.choose id

let partOne (input:array<string>) =
    let countedChars = countChars input

    let gamma =
        countedChars
        |> mostCommon
        |> String.Concat

    let epislon =
        countedChars
        |> leastCommon
        |> String.Concat

    (fromBinary gamma) * (fromBinary epislon)

assert (partOne testInput = 198)

let input = File.ReadAllLines "day03/input.txt"
printfn $"{partOne input}"