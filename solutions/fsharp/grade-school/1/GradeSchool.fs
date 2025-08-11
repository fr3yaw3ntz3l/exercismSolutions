module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School = 
    let studentExistsInSchool = 
        school 
        |> Map.exists (fun _ students -> List.contains student students)
    
    if studentExistsInSchool then 
        school
    else 
        let existingStudents = Map.tryFind grade school |> Option.defaultValue []
        let updatedStudents = student :: existingStudents
        Map.add grade updatedStudents school

let roster (school: School): string list = 
    school 
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.map snd
    |> Seq.map List.sort
    |> Seq.concat
    |> List.ofSeq
    
let grade (number: int) (school: School): string list = 
    Map.tryFind number school 
    |> Option.defaultValue [] 
    |> List.sort

