module EliudsEggs

let eggCount n = 
    let rec eggToList n lst =
        match n with
        | 0 -> lst
        | x when x % 2 = 1 -> eggToList ((x - 1) / 2) (1 :: lst)
        | x when x % 2 = 0 -> eggToList (x / 2) lst
    
    let eggList = eggToList n []
    List.length eggList

    