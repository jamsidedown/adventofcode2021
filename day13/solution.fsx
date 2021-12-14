open System.IO
open System.Text.RegularExpressions

let foldPattern = Regex "^fold along (x|y)=(\\d+)$"
let coordPattern = Regex "^(\\d+),(\\d+)$"

type Fold =
    | X of int
    | Y of int

let readInput (filepath:string) =
    let lines = File.ReadAllLines filepath

    let coords =
        lines
        |> Array.filter coordPattern.IsMatch
        |> Array.map (fun line ->
            line.Split ','
            |> Array.map int
            |> function
                | [| a; b |] -> Some (a, b)
                | _ -> None)
        |> Array.choose id

    let folds = 
        lines
        |> Array.filter foldPattern.IsMatch
        |> Array.map foldPattern.Match
        |> Array.map (fun group ->
            group.Groups
            |> Seq.map (fun g -> g.Value)
            |> Seq.toArray
            |> function
                | [| _; "x"; n |] -> Some (X (int n))
                | [| _; "y"; n |] -> Some (Y (int n))
                | _ -> None)
        |> Array.choose id
        |> Array.toList

    (coords, folds)

let fold (coords:array<int*int>) (fold:Fold) =
    match fold with
    | X n ->
        coords
        |> Array.map (fun (x, y) ->
            match x with
            | x when x > n -> (x - (2 * (x - n)), y)
            | _ -> (x, y))
    | Y n ->
        coords
        |> Array.map (fun (x, y) ->
            match y with
            | y when y > n -> (x, y - (2 * (y - n)))
            | _ -> (x, y))

let pprint (coords: array<int*int>) =
    let lookup = Set.ofArray coords
    let maxX = coords |> Array.maxBy fst |> fst
    let maxY = coords |> Array.maxBy snd |> snd
    let grid =
        [| 0..maxY |]
        |> Array.map (fun y ->
            [| 0..maxX |]
            |> Array.map (fun x ->
                match lookup.Contains (x, y) with
                | true -> "#"
                | false -> " ")
            |> String.concat "")
    for line in grid do
        printfn "%s" line

let partOne (coords:array<int*int>) (folds:list<Fold>) =
    let f = folds |> List.head
    fold coords f |> Array.distinct |> Array.length

let partTwo (coords:array<int*int>) (folds:list<Fold>) =
    let rec recurse (coords:array<int*int>) (folds:list<Fold>) =
        match folds with
        | f :: fs ->
            let next = fold coords f |> Array.distinct
            recurse next fs
        | [] -> coords

    recurse coords folds

let (testCoords, testFolds) = readInput "day13/testInput.txt"
assert (partOne testCoords testFolds = 17)

let (coords, folds) = readInput "day13/input.txt"
printfn $"{partOne coords folds}"
pprint (partTwo coords folds)
