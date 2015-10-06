module RosalindStepDefinition

open TickSpec
open NUnit.Framework
open System

let mutable dnaString : string = ""  
      
let [<Given>] ``Given a DNA string "(.*)"``(s : string) =  
    dnaString <- s
            
let [<Then>] ``Then the transcribed RNA string should be (.*)``(s : string) =         
    Assert.True((dnaString = s))
