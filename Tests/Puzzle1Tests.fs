namespace FSharp.Puzzles.Tests

open System
open FSharp.Puzzles.Solutions
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit
open FsUnit.MsTest

[<TestClass>]
type ``Puzzle 1: Take the Nth Element from the tail``() = 
    [<TestMethod>]
    member this.``Puzzle 1: try taking different nth elements in a test list``() = 
        let input_dataset = [2;3;4;5;6;7;8;9;10;11]
        let input_index = [1;5;2;10;12;-1;0]
        
        let expected = [Some(11);Some(7);Some(10);
                        Some(2);None;None;None;]
                     
        input_index |> List.map(
                        fun i -> Puzzle1.TakeNthElementFromTail(input_dataset, i))
                    |> should equal expected
                
        Puzzle1.TakeNthElementFromTail([], 3) 
        |> should equal None         
        


