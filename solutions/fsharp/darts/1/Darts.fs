module Darts

open System 

let score (x: float) (y: float) : int = 
    let radius = Math.Sqrt(Math.Pow(x, 2.0) + Math.Pow(y, 2.0))
    if radius > 10.0 then 0
    elif radius > 5.0 then 1
    elif radius > 1.0 then 5
    else 10
