module Yacht

type Category = 
    | Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht

type Die =
    | One 
    | Two 
    | Three
    | Four 
    | Five 
    | Six

let dieNum dice = 
    match dice with
    | One -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6

let score category dice = 
    match category with
    | Ones ->
        dice 
        |> List.filter (fun x -> x = Die.One) 
        |> List.map dieNum 
        |> List.sum
    | Twos -> 
        dice 
        |> List.filter (fun x -> x = Die.Two) 
        |> List.map dieNum 
        |> List.sum
    | Threes -> 
        dice 
        |> List.filter (fun x -> x = Die.Three) 
        |> List.map dieNum 
        |> List.sum
    | Fours -> 
        dice 
        |> List.filter (fun x -> x = Die.Four) 
        |> List.map dieNum 
        |> List.sum
    | Fives -> 
        dice 
        |> List.filter (fun x -> x = Die.Five) 
        |> List.map dieNum 
        |> List.sum
    | Sixes -> 
        dice 
        |> List.filter (fun x -> x = Die.Six) 
        |> List.map dieNum 
        |> List.sum
    | FullHouse ->
        let groups = 
            dice
            |> List.countBy id
            |> List.map snd
        if List.sort groups = [2; 3] then
            dice 
            |> List.map dieNum 
            |> List.sum
        else 0
    | FourOfAKind ->
        let groups = 
            dice
            |> List.countBy id
        let hasFourOrMore = 
            groups 
            |> List.exists (fun (_, count) -> count >= 4)
        if hasFourOrMore then
            let (die, _) = 
                groups 
                |> List.find (fun (_, count) -> count >= 4)
            (dieNum die) * 4
        else 0
    | LittleStraight ->
        if List.sort dice = [Die.One; Die.Two; Die.Three; Die.Four; Die.Five] 
        then 30 
        else 0
    | BigStraight ->
        if List.sort dice = [Die.Two; Die.Three; Die.Four; Die.Five; Die.Six] 
        then 30 
        else 0
    | Choice ->
        dice 
        |> List.map (fun x -> dieNum x) 
        |> List.sum
    | Yacht ->
        let head = List.head dice
        if List.forall (fun x -> x = head) dice 
        then 50 
        else 0
        