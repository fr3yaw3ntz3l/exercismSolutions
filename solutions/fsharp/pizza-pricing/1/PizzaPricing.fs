module PizzaPricing

type Pizza =
    | Margherita
    | Caprese
    | Formaggio
    | ExtraSauce of Pizza
    | ExtraToppings of Pizza

let rec pizzaPrice (pizza: Pizza): int = 
    match pizza with
    | Margherita -> 7
    | Caprese -> 9
    | Formaggio -> 10
    | ExtraSauce p -> 1 + pizzaPrice p
    | ExtraToppings p -> 2 + pizzaPrice p

let extraCharge(pizzas: Pizza list): int =
    match List.length pizzas with
    | 1 -> 3
    | 2 -> 2
    | _ -> 0

let rec orderPrice(pizzas: Pizza list): int = 
    let pizzaTotal = List.sumBy pizzaPrice pizzas
    let extra = extraCharge pizzas
    pizzaTotal + extra
