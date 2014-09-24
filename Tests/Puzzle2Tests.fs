namespace FSharp.Puzzles.Tests

open System
open FSharp.Puzzles.Solutions
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit
open FsUnit.MsTest

[<TestClass>]
type ``Puzzle 2: Determine triangle type from the lengths of the sides``() =
    [<TestMethod>]
    member this.``Puzzle 2: Test various combinations of side lengths``() =       
             let input = [[1.0F; 1.0F; 1.0F];
                          [2.2F; 2.2F; 2.2F];
                          [1.0F; 2.0F; 1.0F];
                          [2.0F; 1.0F; 1.0F];
                          [5.0F; 4.0F; 3.0F];         
                          [1.0F; 1.0F; 1.0F; 2.0F]]; 
            
             let expected = [Equilateral;Equilateral;Isosceles;
                             Isosceles;Scalene;Error]
             
             input |> List.map(
                        fun x -> Puzzle2.GetTriangleType(x))
                   |> should equal expected

