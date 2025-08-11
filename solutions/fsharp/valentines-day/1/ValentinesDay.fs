module ValentinesDay

type Approval =
    | Yes
    | No 
    | Maybe

type Cuisine =
    | Korean
    | Turkish

type Genre =
    | Crime
    | Horror
    | Romance
    | Thriller

type Activity =
    | BoardGame
    | Chill
    | Movie of Genre
    | Restaurant of Cuisine
    | Walk of int 

let rateActivity (activity: Activity): Approval = 
    match activity with
    | BoardGame -> No 
    | Chill -> No 
    | Movie Genre ->
        match Genre with
        | Romance -> Yes
        | _ -> No 
    | Restaurant Cuisine ->
        match Cuisine with
        | Korean -> Yes
        | Turkish -> Maybe
    | Walk x ->
        match x with
        | x when x < 3 -> Yes
        | x when x < 5 -> Maybe
        | _ -> No
