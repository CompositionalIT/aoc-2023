#load "common.fsx"
open System

type Card =
    | A = 14
    | K = 13
    | Q = 12
    | J = 11
    | T = 10

type Hand = Card list

type Classification =
    | FiveOfAKind = 6
    | FourOfAKind = 5
    | FullHouse = 4
    | ThreeOfAKind = 3
    | TwoPair = 2
    | OnePair = 1
    | HighCard = 0

module Card =
    let jokerComparer a b =
        match a, b with
        | Card.J, Card.J -> 0
        | Card.J, _ -> -1
        | _, Card.J -> 1
        | _, _ -> compare a b

module Hand =
    type ComparisonMode =
        | NormalComparer
        | JokerComparer

    let classify hand =
        match hand |> List.countBy id |> List.map snd |> List.sortDescending with
        | 5 :: _ -> Classification.FiveOfAKind
        | 4 :: _ -> Classification.FourOfAKind
        | 3 :: 2 :: _ -> Classification.FullHouse
        | 3 :: _ -> Classification.ThreeOfAKind
        | 2 :: 2 :: _ -> Classification.TwoPair
        | 2 :: _ -> Classification.OnePair
        | 1 :: _ -> Classification.HighCard
        | _ -> failwith "Invalid hand"

    let (|Equal|Unequal|) =
        function
        | 0 -> Equal
        | x -> Unequal x

    let compare comparisonMode (h1: Hand) h2 =
        match compare (classify h1) (classify h2) with
        | Equal ->
            List.zip h1 h2
            |> List.map (fun (a, b) ->
                match comparisonMode with
                | NormalComparer -> compare a b
                | JokerComparer -> Card.jokerComparer a b)
            |> List.tryFind ((<>) 0)
            |> Option.defaultValue 0
        | Unequal x -> x

type Line = { Hand: Hand; Bid: int }

module Line =
    let compare mode a b = Hand.compare mode a.Hand b.Hand

let (|Chars|) (s: string) = s.ToCharArray() |> Array.toList

let (|ACard|_|) (c: char) =
    match string c with
    | "A" -> Some(Card.A)
    | "K" -> Some(Card.K)
    | "Q" -> Some(Card.Q)
    | "J" -> Some(Card.J)
    | "T" -> Some(Card.T)
    | Int c -> Some(enum<Card> c)
    | _ -> None

let parse line =
    match line with
    | Split ' ' [ Chars [ ACard a; ACard b; ACard c; ACard d; ACard e ]; Int bid ] -> {
        Hand = [ a; b; c; d; e ]
        Bid = bid
      }
    | _ -> failwith "Invalid line"

let sample =
    """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""

let parsedSample = sample.ByNewLine() |> Array.map parse
let parsedFile = Files[7] |> Array.map parse

// Part 1
let calculateWinnings lines =
    lines
    |> Array.sortWith (Line.compare Hand.NormalComparer)
    |> Array.indexed
    |> Array.map (fun (i, l) -> i + 1, l)
    |> Array.sumBy (fun (rank, line) -> rank * line.Bid)

parsedSample |> calculateWinnings
parsedFile |> calculateWinnings

// Part 2
type JokerLine = {
    Actual: Hand
    Simulated: Hand
    Bid: int
} with

    static member OfLine(l: Line) = {
        Actual = l.Hand
        Simulated = l.Hand
        Bid = l.Bid
    }

module JokerLine =
    let compare (j1: JokerLine) (j2: JokerLine) =
        match compare (Hand.classify j1.Simulated) (Hand.classify j2.Simulated) with
        | Hand.Equal ->
            List.zip j1.Actual j2.Actual
            |> List.map (fun (a, b) -> Card.jokerComparer a b)
            |> List.tryFind ((<>) 0)
            |> Option.defaultValue 0
        | Hand.Unequal x -> x

    let compareSimulated a b =
        Hand.compare Hand.JokerComparer a.Simulated b.Simulated

    let compareActual a b =
        Hand.compare Hand.NormalComparer a.Actual b.Actual

    let crossJoin xss =
        let rec outerListLoop acc xs =
            match xs with
            | [] -> acc
            | x :: rest -> innerListLoop acc x rest

        and innerListLoop acc ys xs =
            match ys with
            | [] -> acc
            | y :: rest ->
                let acc' = (y :: List.head acc) :: List.tail acc
                let os = outerListLoop acc' xs
                let is = innerListLoop acc rest xs
                os @ if rest |> List.isEmpty then List.tail is else is

        let xss' = xss |> List.filter (fun xs -> List.length xs > 0)
        outerListLoop [ [] ] xss'

    let findBestJoker joker =
        let jokers =
            joker.Actual |> List.indexed |> List.filter (snd >> (=) Card.J) |> List.map fst

        [
            for index in jokers do
                [ 2..14 ] |> List.map (fun c -> index, enum<Card> c)
        ]
        |> crossJoin
        |> List.map (fun mods ->
            (joker, mods)
            ||> List.fold (fun line (index, card) -> {
                line with
                    Simulated = line.Simulated |> List.updateAt index card
            }))
        |> List.sortWith compareSimulated
        |> List.last

let calculateWinningsJokers lines =
    lines
    |> Array.Parallel.map (JokerLine.OfLine >> JokerLine.findBestJoker)
    |> Array.sortWith JokerLine.compare
    |> Array.indexed
    |> Array.Parallel.map (fun (i, l) -> i + 1, l)
    |> Array.sumBy (fun (rank, line) -> rank * line.Bid)

parsedSample |> calculateWinningsJokers
parsedFile |> calculateWinningsJokers