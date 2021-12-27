#load "Helpers.fsx"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let serial = Helpers.Web.getInput 11 |> Array.head |> int64

let power (x,y) =
    let rackId = x + 10L
    rackId * y
    |> ((+)serial)
    |> ((*)rackId)
    |> (fun n -> (n / 100L) % 10L)
    |> (fun n -> n - 5L)

let sq =
    Array.allPairs [|1L..300L|] [|1L..300L|]
    |> Array.map (fun c -> c,power c)
    |> Map.ofArray

let squares (x,y) = [|(x,y);(x+1L,y);(x+2L,y)
                      (x,y+1L);(x+1L,y+1L);(x+2L,y+1L)
                      (x,y+2L);(x+1L,y+2L);(x+2L,y+2L)
                    |]

let ans1 =
    Array.allPairs [|1L..298L|] [|1L..298L|]
    |> Array.map (fun c ->
        c, squares c |> Array.sumBy (fun c' -> sq[c'])
        )
    |> Array.maxBy snd
    |> fst

let power2 x' y' =
    let (x,y) = (int64 x')+1L,(int64 y')+1L
    let rackId = x + 10L
    rackId * y
    |> ((+)serial)
    |> ((*)rackId)
    |> (fun n -> (n / 100L) % 10L)
    |> (fun n -> n - 5L)

let powArr = Array2D.init 300 300 power2

let calcSq (x,y) size prevCalc =
    let mutable sum = prevCalc
    for x' in 0..(size-2) do
        sum <- sum + powArr[x+x', y+size-1]
    for y' in 0..(size-2) do
        sum <- sum + powArr[x+size-1, y+y']

    sum <- sum + powArr[x+size-1,y+size-1]

    sum

let ans2 =
    Array.allPairs [|1..300|] [|1..300|]
    |> Array.map (fun (x,y) -> (x-1,y-1), min (300-x+1) (300-y+1))
    |> Array.collect (fun (c,maxsize) ->
        //printfn "%A" c
        [|1..maxsize|]
        |> Array.scan (fun (_,_,state) size -> c, size, calcSq c size state) ((-1,-1),0,0L)
        )
    |> Array.maxBy (fun (_,_,sum) -> sum)
    |> (fun ((x,y),size,_) -> printfn "%i,%i,%i" (x+1) (y+1) size)


(*
  (x,y) = 1,1
  1 2 3 4
1 x x
2 x x
3
4

*)