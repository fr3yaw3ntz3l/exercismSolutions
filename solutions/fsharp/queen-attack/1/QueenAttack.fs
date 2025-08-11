module QueenAttack

let create (position: int * int) = 
    let (column, row) = position
    let validPosition n = n >= 0 && n <= 7
    validPosition column && validPosition row

let canAttack (queen1: int * int) (queen2: int * int) = 
    let (q1Column, q1Row) = queen1
    let (q2Column, q2Row) = queen2
    q1Column = q2Column ||
    q1Row = q2Row ||
    abs (q1Column - q2Column) = abs (q1Row - q2Row)