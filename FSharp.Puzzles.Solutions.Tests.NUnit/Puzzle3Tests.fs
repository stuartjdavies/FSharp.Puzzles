module FSharp.Puzzles.Solutions.Tests.NUnit.Puzzle3Tests

open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote
open FSharp.Puzzles.Solutions
open LoremNET

[<Test>]
let ``Puzzle 3: try reversing words in different sentences``() =                                      
            let input = ["Hello I am here";"Cat and dog";"I am here";"There is a  apple here  ";""]         
            let expected = ["olleH I ma ereh";"taC dna god";"I ma ereh";"erehT si a  elppa ereh  ";""]

            input |> List.map(fun p -> Puzzle3.reverseWordsInString(p)) |> should equal expected

