open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    File.ReadAllLines("input6.txt")
    |> Array.map (fun s -> s.Split([|", "|], StringSplitOptions.None))
    |> Array.map (fun [|a1;a2|] -> (int a1,int a2))

let padding = 0

let newCoords =
    data
    |> Array.map (fun (a1,a2) -> a1+padding,a2+padding)

let maxX, maxY = (Array.maxBy fst newCoords |> fst, Array.maxBy snd newCoords |> snd)

let (gridX, gridY) = (maxX + padding, maxY + padding)

let mh (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

let distTo x =
    newCoords
    |> Array.mapi (fun i y -> i, mh x y)
    |> Array.groupBy snd
    |> Array.minBy fst
    |> function
       | _, [| (i,_) |] -> Some i
       | _ -> None

seq {
    for x in 0 .. gridX do
        for y in 0 .. gridY do
            yield distTo (x,y)
}
|> Seq.choose id
|> Array.ofSeq
|> Array.groupBy id
|> Array.map (fun (i,a) -> i, Array.length a)
|> Array.sortByDescending snd

// Part 2

let sumOfDists x =
    newCoords
    |> Array.sumBy (mh x)

seq {
    for x in 0 .. gridX do
        for y in 0 .. gridY do
            yield sumOfDists (x,y)
}
|> Seq.toArray
|> Array.filter (fun x -> x < 10_000)
|> Array.length