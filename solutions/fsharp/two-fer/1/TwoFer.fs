module TwoFer

let twoFer (input: string option): string = 
    let x =
        match input with
        | Some name -> name
        | None -> "you"
    sprintf "One for %s, one for me." x