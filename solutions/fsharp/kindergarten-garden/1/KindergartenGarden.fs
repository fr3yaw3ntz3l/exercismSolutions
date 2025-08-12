module KindergartenGarden

type Plant =
    | Grass
    | Clover
    | Radishes
    | Violets

let charToPlant c =
    match c with
    | 'G' -> Grass
    | 'C' -> Clover
    | 'R' -> Radishes
    | 'V' -> Violets
    | _ -> failwith "Invalid plant character"

let splitDiagram (diagram : string) =
    let parts = diagram.Split('\n')
    let row1 = parts[0]
    let row2 = parts[1]
    (row1, row2)

let students = ["Alice"; "Bob"; "Charlie"; "David"; "Eve"; "Fred"; 
                "Ginny"; "Harriet"; "Ileana"; "Joseph"; "Kincaid"; "Larry"]

let studentToIndex student =
    List.findIndex ((=) student) students

let getPlantPositions studentIndex =
    let pos1 = studentIndex * 2
    let pos2 = studentIndex * 2 + 1
    (pos1, pos2)

let plants diagram student = 
    let studentIndex = studentToIndex student
    let (pos1, pos2) = getPlantPositions studentIndex
    let (row1, row2) = splitDiagram diagram
    
    let plant1 = row1.[pos1]
    let plant2 = row1.[pos2]
    let plant3 = row2.[pos1]
    let plant4 = row2.[pos2]
    
    [charToPlant plant1; charToPlant plant2; charToPlant plant3; charToPlant plant4]
