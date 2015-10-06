module FSharp.Puzzles.Tests.``Puzzle 5: Closest Numbers``

open System
open FSharp.Puzzles.Solutions
open FsUnit
open NUnit.Framework

let splitIntoPairs (xs : int array) = 
         let pairs = xs |> Seq.mapi(fun i item -> i / 2, item)
                        |> Seq.groupBy(fun (g, _) -> g)
                        |> Seq.map(fun (g, xs) -> xs |> Seq.map(fun (_, x) -> x) |> Seq.toList)
                        |> Seq.toList
                        |> List.map(fun item -> match item with
                                                | [x;y] -> (x,y) 
                                                | _ -> failwith "invalid expected")                        
         pairs                                   

[<TestCase([|-20; -3916237; -357920; -3620601; 7374819; -7330761; 30; 6246457; -6461594; 266854|], [|-20;30;|], 50)>]
let ``Verify sample input vs sample output``(input : int array, rawExpected : int array, diff : int) =                                                                                     
          input |> Seq.toList |> Puzzle5.getPairsWithSmallestDiff |> should equal ((splitIntoPairs rawExpected), diff)


