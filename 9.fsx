#load "common.fsx"
open System

let parse lines =
    lines
    |> Array.map (function
        | Split ' ' numbers -> numbers |> List.map int)

// Part 1
let findNextNumber line =
    let rec doIt numbers =
        match numbers with
        | [] -> failwith "error"
        | head :: tail ->
            if head |> List.forall ((=) 0) then
                tail
            else
                let newHead = head |> List.pairwise |> List.map (fun (a, b) -> b - a)
                let pyramid = doIt (newHead :: head :: tail)

                match pyramid with
                | [] -> failwith "error"
                | head :: [] -> [ head ]
                | head :: second :: tail -> ((second @ [ (List.last second) + List.last head ]) :: tail)

    doIt [ line ] |> List.concat |> List.last

let sample =
    """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"""

let parsedSample = sample.ByNewLine() |> parse
let parsedFile = Files[9] |> parse

parsedSample |> Array.map findNextNumber |> Array.sum

// Part 2
let findPreviousNumber line =
    let rec doIt numbers =
        match numbers with
        | [] -> failwith "error"
        | head :: tail ->
            if head |> List.forall ((=) 0) then
                tail
            else
                let newHead = head |> List.pairwise |> List.map (fun (a, b) -> b - a)
                let pyramid = doIt (newHead :: head :: tail)

                match pyramid with
                | [] -> failwith "error"
                | head :: [] -> [ head ]
                | head :: second :: tail -> (((List.head second - List.head head) :: second) :: tail)

    doIt [ line ] |> List.concat |> List.head

parsedSample |> Array.map findPreviousNumber |> Array.sum
parsedFile |> Array.map findPreviousNumber |> Array.sum