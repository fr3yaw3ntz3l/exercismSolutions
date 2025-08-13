module SquareRoot

let squareRoot n =
    let rec aux n m count = 
        if n - m = 0 then
            count + 1
        else
            let newN = n - m
            let newM = m + 2
            aux newN newM (count + 1)
    aux n 1 0
