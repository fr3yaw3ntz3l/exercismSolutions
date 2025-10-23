module Anagram

let findAnagrams (sources : string list) (target : string) : string list = 
    let tLow = target.ToLower()
    let rec aux (lst : string list) (acc : string list) =
        match lst with
        | [] -> acc
        | x :: xs ->
            let xLow = x.ToLower()
            if xLow = tLow then
                aux xs acc
            else
                let xSorted = xLow |> Seq.toList |>  List.sort
                let tSorted = tLow |> Seq.toList |>  List.sort
                if xSorted = tSorted then
                    aux xs (x :: acc)
                else aux xs acc
    List.rev (aux sources [])