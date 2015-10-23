module FSharp.Puzzles.Solutions.Puzzle8

open System
open FSharp.Linq
open FSharp.Puzzles.Solutions.Rop

//
// A Student can be enrolled in 0..4 subjects
// A Subject can have many students
// A Enrolment has a Student and a Subject, it is a join table.    
// A Subject and be either Programming, Design or Literature
// 
type Student = { StudentNumber : string; Ranking : int; IsInternationalStudent : bool; HasScholarship : bool }
type SubjectRecord = { Name : string; MaxSubjectSize: int }
type Subject = | Programming of SubjectRecord | Design of SubjectRecord | Literature of SubjectRecord 
type Enrolment = { StudentNumber : string; SubjectName : string } 
type EnrolmentConfig = { ``Validation Rule 3 - Percentage capacity`` : float; ``Validation Rule 3 - Accepted Ranking`` : int
                         ``Validation Rule 4 - Percentage capacity`` : float; ``Validation Rule 5 - Percentage capacity`` : float; }

type ValidationFailure = 
        | ``Failed validation Rule 1 - Students have an entrance ranking between 0 and 100``
        | ``Failed Validation Rule 2 - All subjects must be below the max student size`` 
        | ``Failed Validation Rule 3 - When programming subjects reach a given percentage capacity then only students are accepted with certain rankings``
        | ``Failed Validation Rule 4 - When literature subjects reach a given percentage capacity, people on scholarships will no longer be accepted`` 
        | ``Failed Validation Rule 5 - When literature subjects reach a given percentage capacity, people on scholarships will no longer be accepted``             
                       
type RandomValueGenerator() =
        let r = new Random()   
        member __.genInt(min, max) = r.Next(min, max)
        member __.genBool() = __.genInt(0, 1) = 1

// Used for picking subjects randomly
type RandomItemPicker() =    
        let r = new RandomValueGenerator()    
        member __.pickItem (items : _ list) =                       
                     let index = r.genInt(0, items.Length)    
                     let _, lst = items |> List.partition (fun item -> item = items.[index])                        
                     items.[index], lst

        member __.pickNItems n (items : _ list) =                                       
                     let rec aux count acc (items : _ list) =                        
                                   if count = 0 then
                                       acc
                                   else
                                       match items with
                                       | [] -> raise(new Exception("Invalid count"))                            
                                       | _ -> let item, lst = __.pickItem items
                                              aux (count - 1) (item::acc) lst                         
                     aux n [] items

type RandomStudentGenerator() =
        let r = RandomValueGenerator()          
        member __.genOne studentNumber =
                    { StudentNumber=studentNumber.ToString(); Ranking=r.genInt(0, 100); 
                      IsInternationalStudent=r.genBool(); HasScholarship=r.genBool() }                    
        member __.genMany numberOfStudents = [ for x in 1 .. numberOfStudents -> __.genOne x ] 

module RandomEnrolmentGenerator =                  
        let generate numberOfStudents numberOfSubjectsPerStudents (subjects : Subject list) =                     
                    let students = numberOfStudents |> (new RandomStudentGenerator()).genMany                     
                    students |> List.map(fun s -> subjects |> (new RandomItemPicker()).pickNItems numberOfSubjectsPerStudents |> List.map(fun sub -> s, sub)) |> List.concat  


type ValidationResult<'T1, 'T2> = 
        | ValidationSucceeded of 'T1
        | ValidationFailed of 'T2
              

module EnrolmentValidator =                                                                                         
        let getPercentageFull (subject : SubjectRecord) (es : Enrolment list) = 
                   ((es |> List.filter(fun e -> e.SubjectName = subject.Name) |> List.length |> Convert.ToDouble) / (float subject.MaxSubjectSize)) * 100.0                                      
        let isSubjectFull(es : Enrolment list, s : SubjectRecord) =  (es |> List.filter(fun e -> e.SubjectName = s.Name) |> List.length) >= s.MaxSubjectSize                
        let isStudentRankingBetween1And100 (student : Student) = (student.Ranking >= 0 && student.Ranking <= 100)        
        let isSubjectAboveCapacity(s : SubjectRecord, es : Enrolment list, capacity : float) = getPercentageFull s es >= capacity     

        let ``Validation Rule 1 - Students have an entrance ranking between 0 and 100`` 
                    (input : Student * Subject * (Enrolment list) * EnrolmentConfig) = 
                    let student, subject, es, c = input                    
                    
                    if isStudentRankingBetween1And100(student) then 
                           Success(input, []) 
                    else 
                           Failure([``Failed validation Rule 1 - Students have an entrance ranking between 0 and 100`` ])
                                                                                       
        let ``Validation Rule 2 - All subjects must be below the max student size`` 
                    (input : Student * Subject * (Enrolment list) * EnrolmentConfig) =                                                            
                    let student, subject, es, c = input    

                    let aux s = if isSubjectFull(es, s) = false then 
                                  Success(input, []) 
                                else 
                                  Failure([ ``Failed Validation Rule 2 - All subjects must be below the max student size`` ])                                                                                                    
                    match subject with
                    | Programming x -> aux x
                    | Design x -> aux x
                    | Literature x -> aux x
                     
        let ``Validation Rule 3 - When programming subjects reach a given percentage capacity then only students are accepted with certain rankings.`` 
                    (input : Student * Subject * (Enrolment list) * EnrolmentConfig) =
                    let student, subject, es, c = input  
                    match subject with
                    | Programming s when isSubjectAboveCapacity(s, es, c.``Validation Rule 3 - Percentage capacity``) && student.Ranking < c.``Validation Rule 3 - Accepted Ranking`` -> 
                                Failure([``Failed Validation Rule 3 - When programming subjects reach a given percentage capacity then only students are accepted with certain rankings``])                    
                    | _ -> Success(input, [])        

        let ``Validation Rule 4 - When design subjects reach a given percentage capacity, only international students will be accepted.`` 
                    (input : Student * Subject * (Enrolment list) * EnrolmentConfig) = 
                    let student, subject, es, c = input 
                    match subject with 
                    | Design s when isSubjectAboveCapacity(s, es, c.``Validation Rule 4 - Percentage capacity``) && (student.IsInternationalStudent = false) -> 
                                Failure([``Failed Validation Rule 4 - When literature subjects reach a given percentage capacity, people on scholarships will no longer be accepted``])                                                                     
                    | _ -> Success(input, [])  
                            
        let ``Validation Rule 5 - When literature subjects reach a given percentage capacity, people on scholarships will no longer be accepted`` 
                    (input : Student * Subject * (Enrolment list) * EnrolmentConfig) = 
                    let student, subject, es, c = input 
                    match subject with
                    | Literature s when isSubjectAboveCapacity(s, es, c.``Validation Rule 5 - Percentage capacity``) && student.HasScholarship = true -> 
                               Failure([``Failed Validation Rule 5 - When literature subjects reach a given percentage capacity, people on scholarships will no longer be accepted`` ])                                                       
                    | _ -> Success(input, [])                   
                                                                             
        let validate input =
                    input |> ``Validation Rule 1 - Students have an entrance ranking between 0 and 100``
                          &&& ``Validation Rule 2 - All subjects must be below the max student size`` 
                          &&& ``Validation Rule 3 - When programming subjects reach a given percentage capacity then only students are accepted with certain rankings.``
                          &&& ``Validation Rule 4 - When design subjects reach a given percentage capacity, only international students will be accepted.``
                          &&& ``Validation Rule 5 - When literature subjects reach a given percentage capacity, people on scholarships will no longer be accepted``
                      
        let validateBatch config (existingEnrolments : Enrolment list) (newEnrolments : (Student * Subject) list)  =    
                    let getSubjectName (s : Subject) = match s with
                                                       | Programming x -> x.Name
                                                       | Design x -> x.Name
                                                       | Literature x -> x.Name                    
                    let rec aux valid invalid enrolments newEs =
                            match newEs with
                            | [] -> valid,invalid
                            | (student,subject)::t -> match validate (student, subject, enrolments, config) with
                                                      |  Success(input, _) -> let xs = (student,subject)::valid
                                                                              let ys = { Enrolment.StudentNumber=student.StudentNumber; SubjectName=getSubjectName(subject) }::enrolments
                                                                              aux xs invalid ys t                                                                                
                                                      |  Failure(msgs) -> let xs = (student,subject,msgs)::invalid
                                                                          let ys = { Enrolment.StudentNumber=student.StudentNumber; SubjectName=getSubjectName(subject) }::enrolments
                                                                          aux valid xs ys t                                        
                    aux [] [] existingEnrolments newEnrolments                    

module EnrolmentErrors = 
             let getSubjectRecord r = 
                    match r with 
                    | Programming x -> x 
                    | Design x -> x 
                    | Literature x -> x

             let convertFailureToString error (student : Student) (subject : Subject) config =
                       let sub = getSubjectRecord subject
                       
                       match error with
                       | ``Failed validation Rule 1 - Students have an entrance ranking between 0 and 100`` ->
                                    sprintf "Can't enrol %s because the maximum number of enrolments has been reached, the maximum students for the subject %s is %d" 
                                                student.StudentNumber sub.Name sub.MaxSubjectSize                      
                       | ``Failed Validation Rule 2 - All subjects must be below the max student size`` ->
                            sprintf "Can't enrol %s because when programming subjects are more than %.0f%% full, the student's entrance ranking must be greater than %d for them to be accepted." 
                                                student.StudentNumber config.``Validation Rule 3 - Percentage capacity`` config.``Validation Rule 3 - Accepted Ranking``
                       | ``Failed Validation Rule 3 - When programming subjects reach a given percentage capacity then only students are accepted with certain rankings`` ->
                                    sprintf "Can't enrol %s because the maximum number of enrolments has been reached, the maximum students for the subject %s is %d" 
                                                student.StudentNumber sub.Name sub.MaxSubjectSize            
                       | ``Failed Validation Rule 4 - When literature subjects reach a given percentage capacity, people on scholarships will no longer be accepted`` ->
                                    sprintf "Can't enrol %s because when design subjects are more than %.0f%% full, only international students will be accepted." student.StudentNumber config.``Validation Rule 4 - Percentage capacity`` 
                       | ``Failed Validation Rule 5 - When literature subjects reach a given percentage capacity, people on scholarships will no longer be accepted`` -> 
                                    sprintf "Can't enrol %s because when design subjects are more than %.0f%% full, only international students will be accepted." 
                                               student.StudentNumber config.``Validation Rule 5 - Percentage capacity``


    
                                               
let ``Execute the workflow given in the puzzle``() =
        let config = { ``Validation Rule 3 - Percentage capacity``= 50.0; 
                       ``Validation Rule 3 - Accepted Ranking`` = 70;
                       ``Validation Rule 4 - Percentage capacity`` = 70.0; 
                       ``Validation Rule 5 - Percentage capacity`` = 50.0; }      

        let subjects = [ Programming({ Name="Java Programming"; MaxSubjectSize=20 })
                         Programming({ Name="C# Programming"; MaxSubjectSize=20 }) 
                         Programming({ Name="PHP Programming"; MaxSubjectSize=20 })
                         Programming({ Name="Graphic Design"; MaxSubjectSize=20 })
                         Design({ Name="Web Design"; MaxSubjectSize=20 })
                         Design({ Name="3D Design"; MaxSubjectSize=20 })
                         Literature({ Name="English Literature"; MaxSubjectSize=10 }) ] 
                      
        let newEnrolments = RandomEnrolmentGenerator.generate 55 4 subjects                                                        
        let success, failed = newEnrolments |> EnrolmentValidator.validateBatch config []                               

        //let insertFailureIntoDb success = success |> mapToSuccessToDbRow |> MyDb.insert str
        //let insertFailureIntoDb success = failure |> mapToSuccessToDbRow |> MyDb.insert str

        let insertFailureIntoDb() = Failure([""])
        let insertSuccessIntoDb() = Success("",[])


        let (<&>) f g = match f() with 
                        | Success _ -> g()  
                        | Failure f -> Failure f

        //let result = insertFailureIntoDb <&> insertFailureIntoDb        

        ()

        
                           
                
//        let validEnrolments = StudentEnrol ler.getListOfValidEnrolments config newEnrolments []
//        let log = StudentEnroller.createNewEnrolmentLog config newEnrolments []
//        
//        log |> List.iter(fun (code, message,_,_) -> printfn "Code - %d, Message - %s" code message |> ignore);
//
//        System.Console.ReadLine() |> ignore
 