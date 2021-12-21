#load "Helpers.fsx"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = Helpers.Web.getInput 8 |> Array.head |> Helpers.split " " |> Array.map int

type Node = | Node of children : Node array * meta : int array

let parse (a : int array) =
    let rec f i =
        let childCount = a[i]
        let metaCount = a[i+1]
        let mutable currIdx = i+2
        let children =
            seq {
                for j in [1..childCount] do
                    let (node, idx) = f currIdx
                    currIdx <- idx
                    yield node
            } |> Array.ofSeq

        let metas = a[currIdx..currIdx-1+metaCount]

        Node (children, metas), currIdx+metaCount

    f 0 |> fst

let rec getMetas node =
    match node with
    | Node (c,m) ->
        let childMetas = c |> Array.collect getMetas
        Array.concat [|childMetas; m|]

let ans1 = parse data |> getMetas |> Array.sum

let rec value node =
    match node with
    | Node ([||], m) -> Array.sum m
    | Node (c,m) ->
        m
        |> Array.map (fun i -> Array.tryItem (i-1) c
                               |> Option.map value
                               |> Option.defaultValue 0)
        |> Array.sum

let ans2 = parse data |> value

