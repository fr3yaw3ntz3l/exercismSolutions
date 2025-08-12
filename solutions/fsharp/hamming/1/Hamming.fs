module Hamming

let distance (strand1: string) (strand2: string): int option = 
    if strand1.Length <> strand2.Length then
        None
    else
        let rec aux s1 s2 counter =
            match s1, s2 with
            | [], [] -> counter
            | x :: xs, y :: ys ->
                if x = y then
                    aux xs ys counter
                else aux xs ys (counter + 1)
        let result = aux (Seq.toList strand1) (Seq.toList strand2) 0
        Some result