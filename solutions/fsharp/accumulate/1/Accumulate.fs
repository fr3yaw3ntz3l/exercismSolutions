module Accumulate

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list = 
    let rec accTail input acc =
        match input with
        | [] -> List.rev acc
        | x :: xs -> accTail xs (func x :: acc)
    accTail input []
