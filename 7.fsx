#load "common.fsx"

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
    let MIN_VALUE, MAX_VALUE = 2, int Card.A

    let jokerComparer a b =
        match a, b with
        | Card.J, Card.J -> 0
        | Card.J, _ -> -1
        | _, Card.J -> 1
        | a, b -> compare a b

module Hand =
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

    let private (|Equal|Unequal|) =
        function
        | 0 -> Equal
        | x -> Unequal x

    let compare comparer (c1: Hand) c2 (h1: Hand) h2 =
        // simulated
        match compare (classify c1) (classify c2) with
        | Equal ->
            List.zip h1 h2
            |> List.map (fun (a, b) -> comparer a b)
            |> List.tryFind ((<>) 0)
            |> Option.defaultValue 0
        | Unequal x -> x

type Line = { Hand: Hand; Bid: int }

module Line =
    let compare a b =
        Hand.compare compare a.Hand b.Hand a.Hand b.Hand

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

let calculateWinnings comparer getBid lines =
    lines
    |> Array.sortWith comparer
    |> Array.indexed
    |> Array.sumBy (fun (rank, line) -> (rank + 1) * getBid line)

// Part 1
let calculateWinningsPart1 = calculateWinnings Line.compare _.Bid

parsedSample |> calculateWinningsPart1
parsedFile |> calculateWinningsPart1

// Part 2
type JokerLine = {
    Actual: Hand
    Simulated: Hand
    Bid: int
}

module JokerLine =
    let private basicJokerComparer getter a b =
        Hand.compare Card.jokerComparer a.Simulated b.Simulated (getter a) (getter b)

    let compareHybrid = basicJokerComparer _.Actual
    let compareSimulated = basicJokerComparer _.Simulated

    let ofLine (l: Line) = {
        Actual = l.Hand
        Simulated = l.Hand
        Bid = l.Bid
    }

    let private crossJoin xss =
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

    let findBestJoker jokerLine =
        let jokerIndices =
            jokerLine.Actual
            |> List.indexed
            |> List.filter (snd >> (=) Card.J)
            |> List.map fst

        [
            for index in jokerIndices do
                [ Card.MIN_VALUE .. Card.MAX_VALUE ] |> List.map (fun c -> index, enum<Card> c)
        ]
        |> crossJoin
        |> List.map (fun simulation ->
            (jokerLine, simulation)
            ||> List.fold (fun jokerLine (index, card) -> {
                jokerLine with
                    Simulated = jokerLine.Simulated |> List.updateAt index card
            }))
        |> List.sortWith compareSimulated
        |> List.last

let calculateWinningsPart2 lines =
    lines
    |> Array.Parallel.map (JokerLine.ofLine >> JokerLine.findBestJoker)
    |> calculateWinnings JokerLine.compareHybrid _.Bid

parsedSample |> calculateWinningsPart2
parsedFile |> calculateWinningsPart2