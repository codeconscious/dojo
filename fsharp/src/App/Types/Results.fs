module Results

module Validation =
    // Source: https://thinkfunctionally.hashnode.dev/result-pipeline-with-map-and-bind-in-f-sharp
    open System

    type Student =
        { Name: string
          Age: int
          RollNo: string }

    // Student -> Result<Student, string>
    let validateName (student: Student) =
        if String.IsNullOrWhiteSpace(student.Name)
        then Error "Name can't be empty!"
        else Ok student

    // Student -> Result<Student, string>
    let validateAge (student: Student) =
        if student.Age < 10 || student.Age > 20
        then Error "Age must be between 10-20!"
        else Ok student

    // Student -> Result<Student, string>
    let validateRollNo (student: Student) =
        if String.IsNullOrWhiteSpace(student.RollNo)
        then Error "Roll number can't be empty!"
        else Ok student

    // Student -> Result<Student, string>
    let validateStudent (student: Student) =
        Ok student
        |> Result.bind validateName
        |> Result.bind validateAge
        |> Result.bind validateRollNo

    // Validation Example 1
    let student =
        { Name = "First Last"
          Age = 10
          RollNo = "Whatever" }

    match validateStudent student with
    | Ok(_) -> "Valid student record."
    | Error(err) -> $"Invalid student record. {err}"
    |> printfn "%s" // Valid student record.

    // Validation Example 2
    let student2 =
        { Name = ""
          Age = 10
          RollNo = "Whatever" } // Invalid student record. Name can't be empty!

    // Validation Example 3
    let student3 =
        { Name = "First Last"
          Age = 5
          RollNo = "Whatever" } // Invalid student record. Age must be between 10-20!

    // Validation Example 4
    let student4 =
        { Name = "First Last"
          Age = 10
          RollNo = "" } // Invalid student record. Roll number can't be empty!
