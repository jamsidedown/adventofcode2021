open System.IO

type Line =
    | Complete
    | Corrupted of char
    | Incomplete of list<char>

let readInput (filepath:string) =
    File.ReadAllLines filepath

let brackets = [('(', ')'); ('[', ']'); ('{', '}'); ('<', '>')] |> dict
let opens = Set.ofList ['('; '{'; '['; '<']
let closes = Set.ofList [')'; '}'; ']'; '>']
let corruptScores = [(')', 3); (']', 57); ('}', 1197); ('>', 25137)] |> dict
let incompleteScores = [(')', 1L); (']', 2L); ('}', 3L); ('>', 4L)] |> dict

let parseLine (line:string) =
    let mutable stack = []

    let rec recurse (input:list<char>) =
        match input with
        | c :: cs when opens.Contains c ->
            stack <- c :: stack
            recurse cs
        | c :: cs when closes.Contains c ->
            match stack with
            | lc :: _ when brackets[lc] = c ->
                stack <- stack.Tail
                recurse cs
            | _ -> Corrupted c
        | _ ->
            match stack with
            | [] -> Complete
            | _ -> Incomplete stack

    line |> Seq.toList |> recurse

let partOne (input:array<string>) =
    input
    |> Array.map parseLine
    |> Array.sumBy (fun result ->
        match result with
        | Corrupted c -> corruptScores[c]
        | _ -> 0)

let rec complete (input:list<char>) =
    match input with
    | [] -> []
    | c :: cs -> brackets[c] :: complete cs

let partTwo (input:array<string>) =
    let scores =
        input
        |> Array.map parseLine
        |> Array.map (fun result ->
            match result with
            | Incomplete ls -> Some ls
            | _ -> None)
        |> Array.choose id
        |> Array.map complete
        |> Array.map (fun line ->
            line
            |> List.fold (fun x y -> (x * 5L) + incompleteScores[y]) 0L)
        |> Array.sort
    scores[scores.Length / 2]

let testInput = readInput "day10/testInput.txt"
assert (partOne testInput = 26397)
assert (partTwo testInput = 288957L)

let input = readInput "day10/input.txt"
printfn $"{partOne input}"
printfn $"{partTwo input}"
