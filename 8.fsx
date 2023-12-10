#load "common.fsx"
open System.Collections.Generic

type Direction =
    | Left
    | Right

type NodeId =
    | NodeId of string

    member this.GhostId =
        match this with
        | NodeId value -> value[2]

    static member Start = NodeId "AAA"
    static member End = NodeId "ZZZ"


type Node = {
    Node: NodeId
    Left: NodeId
    Right: NodeId
}

type Network = {
    Directions: Direction list
    Nodes: IReadOnlyDictionary<NodeId, Node>
}

let parse lines =
    ({| Directions = []; Nodes = Map [] |}, lines)
    ||> Array.fold (fun state line ->
        match line with
        | Split '=' [ node; Split ',' [ left; right ] ] -> {|
            state with
                Nodes =
                    state.Nodes.Add(
                        NodeId node,
                        {
                            Node = NodeId node
                            Left = NodeId left[1..]
                            Right = NodeId right[0..2]
                        }
                    )
          |}
        | "" -> state
        | Chars directions -> {|
            state with
                Directions = [
                    for direction in directions do
                        match direction with
                        | 'R' -> Right
                        | 'L' -> Left
                        | _ -> failwith "Invalid direction"
                ]
          |})
    |> fun state -> {
        Nodes = state.Nodes |> Map.toSeq |> readOnlyDict
        Directions = state.Directions
    }

let createInfiniteDirections (directions: _ list) =
    Seq.initInfinite (fun index -> directions[(index % List.length directions)])

let nextDirection network =
    createInfiniteDirections network.Directions
    |> fun x ->
        let enumerator = x.GetEnumerator()

        fun () ->
            enumerator.MoveNext() |> ignore
            enumerator.Current

// Part 1
let calculateSteps network =
    {|
        Position = NodeId.Start
        Network = network
        NextDirection = nextDirection network
    |}
    |> Seq.unfold (fun state ->
        match state.Position with
        | NodeId "ZZZ" -> None
        | _ ->
            let direction = state.NextDirection()
            let currentNode = state.Network.Nodes[state.Position]

            Some(
                (state.Position, direction),
                {|
                    state with
                        Position =
                            match direction with
                            | Left -> currentNode.Left
                            | Right -> currentNode.Right
                |}
            ))


let sample =
    """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"""

let sampleTwo =
    """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"""

let parsedSample = sample.ByNewLine() |> parse
let parsedSampleTwo = sampleTwo.ByNewLine() |> parse
let parsedFile = Files[8] |> parse

calculateSteps parsedFile |> Seq.length

// Part 2
let calculateStepsMultiple network =
    let startNodes =
        network.Nodes
        |> Seq.filter (fun r -> r.Key.GhostId = 'A')
        |> Seq.map (fun r -> r.Key)
        |> Seq.take 3
        |> Seq.toList

    {|
        Positions = startNodes
        Network = network
        NextDirection = nextDirection network
    |}
    |> Seq.unfold (fun state ->
        match state.Positions with
        | nodes when nodes |> Seq.forall (fun node -> node.GhostId = NodeId.End.GhostId) -> None
        | _ ->
            let direction = state.NextDirection()

            let positions = [
                for position in state.Positions do
                    let currentNode = state.Network.Nodes[position]

                    match direction with
                    | Left -> currentNode.Left
                    | Right -> currentNode.Right
            ]

            Some((state.Positions, direction), {| state with Positions = positions |}))

let sampleThree =
    """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"""

let parsedSampleThree = sampleThree.ByNewLine() |> parse
calculateStepsMultiple parsedFile |> Seq.length
let network = parsedFile