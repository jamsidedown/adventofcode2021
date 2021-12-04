open System.IO

type CardNumber =
    | Uncalled of int
    | Called

let readInput (filepath:string) =
    let lines = File.ReadAllLines filepath
    let called = lines[0].Split ',' |> Array.map int
    let bingoCards =
        lines[2..]
        |> Array.filter (fun line -> line.Length > 0)
        |> Array.map (fun line ->
            line.Split ' '
            |> Array.filter (fun split -> split.Length > 0)
            |> Array.map int
            |> Array.map Uncalled)
        |> Array.chunkBySize 5
    (called, bingoCards)

let call (number:int) (card:array<array<CardNumber>>) =
    card
    |> Array.map (fun row ->
        row
        |> Array.map (fun cell ->
            match cell with
            | Uncalled n when n = number -> Called
            | _ -> cell))

let bingo (card:array<array<CardNumber>>) =
    let rowBingo (row:array<CardNumber>) =
        row
        |> Array.filter (fun cell -> cell = Called)
        |> Array.length = 5

    let rowWin =
        card
        |> Array.filter rowBingo
        |> Array.length > 0

    let colWin =
        [| 0..4 |]
        |> Array.map (fun x ->
            card
            |> Array.map (fun row -> row[x]))
        |> Array.filter rowBingo
        |> Array.length > 0

    rowWin || colWin

let play (numbers:array<int>) (card:array<array<CardNumber>>) =
    let rec inner (numbers:list<int>) (card:array<array<CardNumber>>) =
        match numbers with
        | n :: ns ->
            match call n card with
            | c when bingo c -> (1, c)
            | c ->
                let (t, cc) = inner ns c
                (1 + t, cc)
        | _ -> (0, card)
    inner (numbers |> Array.toList) card

let sumUncalled (card:array<array<CardNumber>>) =
    card
    |> Array.sumBy (fun row ->
        row
        |> Array.sumBy (fun cell ->
            match cell with
            | Uncalled n -> n
            | _ -> 0))

let partOne (numbers:array<int>) (cards:array<array<array<CardNumber>>>) =
    let (turns, card) =
        cards
        |> Array.map (play numbers)
        |> Array.sortBy fst
        |> Array.head

    (sumUncalled card) * (numbers[turns - 1])

let partTwo (numbers:array<int>) (cards:array<array<array<CardNumber>>>) =
    let (turns, card) =
        cards
        |> Array.map (play numbers)
        |> Array.sortByDescending fst
        |> Array.head

    (sumUncalled card) * (numbers[turns - 1])

let (testNumbers, testCards) = readInput "day04/testInput.txt"
assert (partOne testNumbers testCards = 4512)
assert (partTwo testNumbers testCards = 1924)

let (numbers, cards) = readInput "day04/input.txt"
printfn $"{partOne numbers cards}"
printfn $"{partTwo numbers cards}"
