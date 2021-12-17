type Target = {
    XMin: int
    XMax: int
    YMin: int
    YMax: int
}

let toTarget (xmin:int) (xmax:int) (ymin:int) (ymax:int) =
    { XMin = xmin; XMax = xmax; YMin = ymin; YMax = ymax }

let inRange (target:Target) (x:int) (y:int) =
    x >= target.XMin && x <= target.XMax && y >= target.YMin && y <= target.YMax

let outOfRange (target:Target) (x:int) (y:int) =
    x >= target.XMax || y <= target.YMin

let passesThrough (target:Target) (ix:int) (iy:int) =
    let rec recurse (x:int) (y:int) (dx:int) (dy:int) =
        match outOfRange target x y with
        | true -> [(x, y)]
        | false ->
            (x, y) :: recurse (x + dx) (y + dy) ([dx - 1; 0] |> List.max) (dy - 1)

    let path = recurse 0 0 ix iy

    match path |> List.filter (fun (x, y) -> inRange target x y) with
    | [] -> None
    | _ -> Some path

let partOne (target:Target) =
    [ 1..20 ]
    |> List.collect (fun ix ->
        [ 1..200 ]
        |> List.map (fun iy -> passesThrough target ix iy))
    |> List.choose id
    |> List.collect (List.map snd)
    |> List.max

let partTwo (target:Target) =
    [ 1..150 ]
    |> List.collect (fun ix ->
        [ -200..200 ]
        |> List.map (fun iy -> passesThrough target ix iy))
    |> List.choose id
    |> List.length

let testInput = toTarget 20 30 -10 -5
let input = toTarget 85 145 -163 -108

assert (partOne testInput = 45)
assert (partTwo testInput = 112)

printfn $"{partOne input}"
printfn $"{partTwo input}"
