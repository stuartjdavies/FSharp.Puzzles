module FSharp.Puzzles.Tests.``Puzzle4: Is it Possible``

open System
open FSharp.Puzzles.Solutions
open FsUnit
open NUnit.Framework

[<TestCase(1,4,5,9, true)>]
[<TestCase(2,2,6,8, true)>]
[<TestCase(2,2,5,9, false)>]
let ``Puzzle 3: Verify sample input vs sample output``(a,b,c,d,result) =                           
          (a,b,c,d) |> Puzzle4.solve |> should equal result