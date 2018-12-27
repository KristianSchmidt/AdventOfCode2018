open System
open System.IO

let input = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\\input2.txt")

// Part 1

let containsLetterXtimes (times : int) (s : string) =
    s.ToCharArray()
    |> Array.groupBy id
    |> Array.map (snd >> Array.length)
    |> Array.contains times

let twos =
    input
    |> Array.where (containsLetterXtimes 2)
    |> Array.length

let threes =
    input
    |> Array.where (containsLetterXtimes 3)
    |> Array.length

let checksum = twos * threes

// Part 2

let charDiff (s1 : string,s2 : string) =
    Array.zip (s1.ToCharArray()) (s2.ToCharArray())
    |> Array.where (fun (c1,c2) -> c1 <> c2)
    |> Array.length

let commonChars =
    seq {
        for s1 in input do
            for s2 in input do
                yield (s1,s2)
    }
    |> Seq.where (charDiff >> (=) 1)
    |> Seq.head
    |> (fun (s1,s2) -> Array.zip (s1.ToCharArray()) (s2.ToCharArray()) |> Array.where (fun (c1,c2) -> c1 = c2))
    |> Array.map fst
    |> String

