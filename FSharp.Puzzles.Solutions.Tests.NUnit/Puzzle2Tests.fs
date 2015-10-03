module FSharp.Puzzles.Solutions.Tests.NUnit.Puzzle2Tests

open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote
open FSharp.Puzzles.Solutions

[<Test>]
let ``Puzzle 2: Test various combinations of side lengths``() =       
             let input = [[1.0F; 1.0F; 1.0F];
                          [2.2F; 2.2F; 2.2F];
                          [1.0F; 2.0F; 1.0F];
                          [2.0F; 1.0F; 1.0F];
                          [5.0F; 4.0F; 3.0F];         
                          [1.0F; 1.0F; 1.0F; 2.0F]]; 
            
             let expected = [Equilateral;Equilateral;Isosceles;
                             Isosceles;Scalene;Error]
             
             input |> List.map(fun x -> Puzzle2.GetTriangleType(x)) |> should equal expected