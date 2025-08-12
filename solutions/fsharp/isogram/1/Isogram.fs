module Isogram
open System

let isIsogram (str: string): bool = 
    let lowerStr = str.ToLower()
    let rec aux chars seen =
        match chars with
        | [] -> true
        | x :: xs ->
            if List.contains x seen then
                false
            else aux xs (x :: seen)
    aux (lowerStr |> Seq.filter Char.IsLetter |> Seq.toList) []