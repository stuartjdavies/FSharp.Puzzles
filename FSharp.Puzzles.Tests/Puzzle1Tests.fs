module FSharp.Puzzles.Tests.``Puzzle 1: Take the Nth Element from the tail``

open System
open FSharp.Puzzles.Solutions
open FsUnit
open NUnit.Framework

[<Test>]
let ``Puzzle 1: Verify example puzzle input equals example puzzle output``() = 
        let input_dataset = [2;3;4;5;6;7;8;9;10;11]
               
        [ input_dataset, 1, Some(11)
          input_dataset, 5, Some(7)
          input_dataset, 2, Some(10)
          input_dataset, 10, Some(2)
          input_dataset, 12, None
          input_dataset,-1, None
          input_dataset, 0, None
          [], 3, None ] 
        |> List.iter(fun (dataset, index, expected) -> (dataset, index) |> Puzzle1.TakeNthElementFromTail |> should equal expected)
          