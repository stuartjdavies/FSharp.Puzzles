module FSharp.Puzzles.Solutions.Tests.NUnit.Puzzle1Tests

open FsUnit
open FsCheck
open NUnit.Framework
open Swensen.Unquote
open FSharp.Puzzles.Solutions

[<Test>]
let ``Puzzle 1: Use FsCheck to compare the puzzle solution to second alternate solution``() =                
        let alternateSolution items (n : int) : _ option = 
                    let index = List.length items - n
                    if index < 0 then None else Some(List.nth items index) 
        
        Check.Quick (fun k n -> let xs = [ 1 .. k ] 
                                (alternateSolution xs n) |> should equal (Puzzle1.takeNthElemFromTail xs n))
                    

