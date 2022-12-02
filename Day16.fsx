#load "Helpers.fsx"

open System
open System.IO
open Helpers

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let parse s =
    match s with
    | Regex "Before: \[(\d), (\d), (\d), (\d)\]" [r0;r1;r2;r3] ->
        [|int r0;int r1;int r2;int r3|]
    | Regex "After:  \[(\d), (\d), (\d), (\d)\]" [r0;r1;r2;r3] ->
        [|int r0;int r1;int r2;int r3|]
    | Regex "(\d+) (\d+) (\d+) (\d+)" [r0;r1;r2;r3] ->
        [|int r0;int r1;int r2;int r3|]

let data =
    Helpers.Web.getInput 16
    |> Array.filter ((<>)"")
    |> Array.chunkBySize 3
    |> Seq.takeWhile (fun arr -> arr[0].StartsWith("Before"))
    |> Array.ofSeq
    |> Array.map (fun [|s1;s2;s3|] -> parse s1, parse s2, parse s3)


// Ops

let opregister op (args : int array) (registers : int array) =
    let res = op registers[args[1]] registers[args[2]]
    let newReg = Array.copy registers
    newReg[args[3]] <- res
    newReg

let opimmediate op (args : int array) (registers : int array) =
    let res = op registers[args[1]] args[2]
    let newReg = Array.copy registers
    newReg[args[3]] <- res
    newReg

let addr = opregister  (+)
let addi = opimmediate (+)

let mulr = opregister  (*)
let muli = opimmediate (*)

let banr = opregister  (&&&)
let bani = opimmediate (&&&)

let borr = opregister  (|||)
let bori = opimmediate (|||)

let setr (args : int array) (registers : int array) =
    let newReg = Array.copy registers
    newReg[args[3]] <- registers[args[1]]
    newReg

let seti (args : int array) (registers : int array) =
    let newReg = Array.copy registers
    newReg[args[3]] <- args[1]
    newReg

let gtir (args : int array) (registers : int array) =
    let newReg = Array.copy registers
    newReg[args[3]] <-
        if args[1] > registers[args[2]] then 1 else 0
    newReg

let gtri (args : int array) (registers : int array) =
    let newReg = Array.copy registers
    newReg[args[3]] <-
        if registers[args[1]] > args[2] then 1 else 0
    newReg

let gtrr (args : int array) (registers : int array) =
    let newReg = Array.copy registers
    newReg[args[3]] <-
        if registers[args[1]] > registers[args[2]] then 1 else 0
    newReg

let eqir (args : int array) (registers : int array) =
    let newReg = Array.copy registers
    newReg[args[3]] <-
        if args[1] = registers[args[2]] then 1 else 0
    newReg

let eqri (args : int array) (registers : int array) =
    let newReg = Array.copy registers
    newReg[args[3]] <-
        if registers[args[1]] = args[2] then 1 else 0
    newReg

let eqrr (args : int array) (registers : int array) =
    let newReg = Array.copy registers
    newReg[args[3]] <-
        if registers[args[1]] = registers[args[2]] then 1 else 0
    newReg

let allFuncs =
    [|addr;addi;mulr;muli;banr;bani;borr;bori;setr;seti
      gtir; gtri; gtrr; eqir; eqri; eqrr;|]

let testEquals (before : int array,args,after : int array) f =
    f args before = after

data
|> Array.map testEquals
|> Array.map (fun f -> allFuncs |> Array.filter f |> Array.length)
|> Array.filter (fun x -> x >= 3)
|> Array.length

// Part 2

let getOpcode (_,y : int array,_) = y[0]

let allFuncs2 =
    [|addr;addi;mulr;muli;banr;bani;borr;bori;setr;seti
      gtir; gtri; gtrr; eqir; eqri; eqrr;|]
    |> Array.mapi (fun i f -> (i,f))

let possibles =
    data
    |> Array.map (fun arr -> getOpcode arr, allFuncs2 |> Array.filter (snd >> (testEquals arr)) |> Array.map fst)
    |> Array.groupBy fst
    |> Array.map (fun (k,arrs) -> k, arrs |> Array.collect snd |> Set.ofArray)

let deduce poss =
    let rec f p res =
        match p with
        | [||] -> res
        | _ ->
            let (k,v) =
                p
                |> Array.find (snd >> Set.count >> ((=)1))
                |> fun (k,s) -> k, Set.maxElement s
            
            let newP = 
                p
                |> Array.filter (fst >> ((<>)k))
                |> Array.map (fun (k,s) -> k, s |> Set.remove v)
            f newP ((k,v) :: res)

    f poss [] |> Map.ofList
        
let instrMap =
    deduce possibles
    |> Map.map (fun k v -> allFuncs[v])

let ans2 =
    (Helpers.Web.getInput 16)[3262..]
    |> Array.map parse
    |> Array.fold (fun regs arr -> instrMap[arr[0]] arr regs) [|0;0;0;0|]

ans2[0]
