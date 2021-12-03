open System
open System.IO

let testInput = [ "00100"; "11110"; "10110"; "10111"; "10101"; "01111"; "00111"; "11100"; "10000"; "11001"; "00010"; "01010"; ]

let toInt (input:seq<char>) =
    Convert.ToInt32(input |> String.Concat, 2)

let partOne (input:list<string>) =
    let rec inner (input:list<list<char>>) =
        let split =
            input
            |> List.map (fun s ->
                match s with
                | c :: cs -> Some (c, cs)
                | _ -> None)
            |> List.choose id

        split
        |> List.map fst
        |> List.countBy id
        |> List.sortByDescending snd
        |> function
            | a :: b :: _ -> (a, b) :: inner (split |> List.map snd)
            | _ -> []

    let processed =
        input
        |> List.map (fun s -> Seq.toList s)
        |> inner

    let gamma =
        processed
        |> Seq.map fst
        |> Seq.map fst
        |> toInt

    let epsilon =
        processed
        |> Seq.map snd
        |> Seq.map fst
        |> toInt

    gamma * epsilon

assert (partOne testInput = 198)

let countFirstChars (input:list<string>) =
    input
    |> List.map (fun s ->
        match Seq.toList s with
        | c :: _ -> Some c
        | _ -> None)
    |> List.choose id
    |> List.countBy id
    |> List.sortByDescending snd

let partTwo (input:list<string>) =
    let inner (predicate:list<string> -> char) (input:list<string>) =
        let rec innerInner (input:list<string>) =
            match List.length input with
            | 1 -> input[0] |> Seq.toList
            | 0 -> []
            | _ ->
                let filterChar = predicate input
                let filtered =
                    input
                    |> List.filter (fun s -> s.StartsWith(filterChar))
                    |> List.map (fun s -> s[1..])
                filterChar :: (innerInner filtered)

        innerInner input

    let genPredicate (input:list<string>) =
        match countFirstChars input with
        | (a, ac) :: (_, bc) :: _ when ac > bc -> a
        | _ -> '1'

    let scrubPredicate (input:list<string>) =
        match countFirstChars input with
        | (_, ac) :: (b, bc) :: _ when ac > bc -> b
        | _ -> '0'

    let gen = input |> inner genPredicate |> toInt
    let scrub = input |> inner scrubPredicate |> toInt

    gen * scrub

assert (partTwo testInput = 230)

let input = File.ReadAllLines "day03/input.txt" |> Array.toList

printfn $"{partOne input}"
printfn $"{partTwo input}"
