namespace FSharp.Puzzles.Tests

open System
open FSharp.Puzzles.Solutions
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit
open FsUnit.MsTest

[<TestClass>]
type ``Small Puzzles: Test case for a list of small puzzles``() =
    [<TestMethod>]   
    member this.``Sum: Verify the sum of a list of numbers``() =                           
            ``List of small puzzles``.Sum([1;2;3;4]) |> should equal 10

    [<TestMethod>]   
    member this.``Multiplication: Verify the multiplication of a list of numbers``() =                           
            ``List of small puzzles``.Multiply([1;2;3;4]) |> should equal 24

    // To be continued ...


