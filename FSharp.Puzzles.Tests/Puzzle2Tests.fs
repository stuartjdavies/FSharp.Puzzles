module FSharp.Puzzles.Tests.``Puzzle 2: Determine triangle type from the lengths of the sides``

open System
open FSharp.Puzzles.Solutions.Puzzle2
open FSharp.Puzzles.Solutions
open FsUnit
open NUnit.Framework

[<TestCase(1.0F, 1.0F, 1.0F)>]
[<TestCase(2.2F, 2.2F, 2.2F)>]
let ``Verify Equilateral is returned when their are three equal sides``(len1, len2, len3) =       
        (len1, len2, len3) |> Puzzle2.getTriangleType |> should equal Equilateral
                
[<TestCase(1.0F, 2.0F, 1.0F)>]
[<TestCase(2.0F, 1.0F, 1.0F)>]
let ``Verify isosceles is returned when there are two equal sides and one unequal side``(len1, len2, len3) =       
         (len1, len2, len3) |> Puzzle2.getTriangleType |> should equal Isosceles

[<TestCase(5.0F, 4.0F, 3.0F)>]
let ``Verify Scalene is returned when there are no equal sides``(len1, len2, len3) =       
         (len1, len2, len3) |> Puzzle2.getTriangleType |> should equal Scalene                 