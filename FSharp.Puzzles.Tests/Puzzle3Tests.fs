module FSharp.Puzzles.Tests.``Puzzle 3: Reverse the words in a string``

open System
open FSharp.Puzzles.Solutions
open FsUnit
open NUnit.Framework

[<TestCase("Hello I am here", "olleH I ma ereh")>]
[<TestCase("Cat and dog", "taC dna god")>]
[<TestCase("I am here", "I ma ereh")>]
[<TestCase("There is a  apple here  ", "erehT si a  elppa ereh  ")>]
[<TestCase("", "")>]
let ``Given a sentence verify that the words are reversed correctly``(input, expected) =                           
          input |> Puzzle3.reverseWordsInString |> should equal expected
          
