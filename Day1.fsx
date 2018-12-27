open System
open System.IO

let input = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\\input1.txt")

let cleanInput = 
    input
    |> Array.map (fun s -> s.Replace("+",""))
    |> Array.map int

// Part 1

let ans =
    cleanInput
    |> Array.sum

// Part 2

let infSeq =
    Seq.initInfinite (fun i -> cleanInput.[i % cleanInput.Length])

let sumSeq =
    infSeq
    |> Seq.scan (+) 0

let scanner (xs : int list) =
    let rec f s =
        function
        | x :: xs ->
            match Set.contains x s with
            | false ->
                f (Set.add x s) xs
            | true ->
                x
        | [] -> failwith ""
    f Set.empty xs

scanner (sumSeq |> Seq.take 1000000 |> List.ofSeq)
