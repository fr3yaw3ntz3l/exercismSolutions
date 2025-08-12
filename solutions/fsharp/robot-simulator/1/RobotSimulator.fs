module RobotSimulator

type Direction = North | East | South | West
type Position = int * int

let create direction position = (direction, position)

let turnRight (dir : Direction) =
    match dir with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let turnLeft (dir : Direction) =
    match dir with
    | North -> West
    | East -> North
    | South -> East
    | West -> South

let advance ((dir, (x, y)) as robot) =
    match dir with
    | North -> (dir, (x, y + 1))
    | East -> (dir, (x + 1, y))
    | South -> (dir, (x, y - 1))
    | West -> (dir, (x - 1, y))

let rec move instructions ((dir, (x, y)) as robot) = 
    let instructionsList = Seq.toList instructions
    match instructionsList with
    | [] -> robot
    | i :: rest ->
        let remainingString = rest |> List.map string |> String.concat ""
        match i with
        | 'R' -> move remainingString (turnRight dir, (x, y))
        | 'L' -> move remainingString (turnLeft dir, (x, y))
        | 'A' -> move remainingString (advance robot)
