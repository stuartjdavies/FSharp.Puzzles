namespace FSharp.Puzzles.Tests

open System
open FSharp.Puzzles.Solutions
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit
open FsUnit.MsTest

[<TestClass>]
type ``Puzzle 3: Reverse the words in a string``() =
    [<TestMethod>]   
    member this.``Puzzle 3: try reversing words in different sentences``() =                           
            let input = ["Hello I am here";"Cat and dog";
                         "I am here";"There is a  apple here  ";""]         
            let expected = ["olleH I ma ereh";"taC dna god";
                            "I ma ereh";"erehT si a  elppa ereh  ";""]

            input |> List.map(
                      fun p -> Puzzle3.ReverseWordsInString(p))
                  |> should equal expected
