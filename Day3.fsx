open System
open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\\input3.txt")

// Part 1

type Claim = { Num : int; X : int; Y : int; Width : int; Length : int }

let regex = Regex("#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$")

let parseClaim (s : string) =
    let grps = regex.Match(s).Groups
    { Num = int (grps.Item 1).Value
      X = int (grps.Item 2).Value
      Y = int (grps.Item 3).Value
      Width = int (grps.Item 4).Value
      Length = int (grps.Item 5).Value }

let claimCoords c =
    seq {
        for i in 0 .. c.Width - 1 do
            for j in 0 .. c.Length - 1 do
                yield (c.Num, c.X + i, c.Y + j)
    }
    |> Array.ofSeq

let claims = input |> Array.map parseClaim

let coordGrps =
    claims
    |> Array.collect claimCoords
    |> Array.groupBy (fun (n,x,y) -> (x,y))
    |> Array.filter (fun (id,arr) -> arr.Length > 1)

coordGrps |> Array.length

// Part 2

let overlappingClaims =
    coordGrps
    |> Array.map snd
    |> Array.collect (Array.map (fun (n,_,_) -> n))
    |> Set.ofArray

let allClaims = Set.ofArray [| 1 .. claims.Length |]

Set.difference allClaims overlappingClaims