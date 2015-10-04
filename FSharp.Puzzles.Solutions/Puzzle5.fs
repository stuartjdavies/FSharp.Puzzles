module FSharp.Puzzles.Solutions.Puzzle5

open System

let getDelta (x : int, y : int) = Math.Abs(if x > y then x - y else y - x) 
let getInputFromLines (line1 : string) (line2 : string) = (int (line1.Trim())), line2.Trim().Split(' ') |> Seq.map(fun item -> (int item));;

let getPairsWithSmallestDiff (lst : int list) = 
      let rec aux xs (diffs : (int * int) list, diff) =                            
                match xs with
                | [] -> diffs, diff 
                | [first] -> diffs, diff
                | first::t ->  t |> List.fold(fun (accDiffs, accDiff) item -> let d = getDelta(first, item) 
                                                                              match accDiffs with
                                                                              | [] -> [ first, item ], d     
                                                                              | _  -> if accDiff > d then [(first,item)], d     
                                                                                      else if (accDiff = d) then (first,item)::accDiffs, d
                                                                                      else if (accDiff < d) then accDiffs, accDiff
                                                                                      else failwith "Invalid case") (diffs, diff) 
                                 |> aux t                                     
      let pairs, diff = aux lst ([], 0)            
      pairs, diff

let getNumberBetweenPairs pairs =      
        pairs |> List.fold(fun acc (f, s) -> f::s::acc) [] 
              |> List.filter(fun num -> pairs |> List.exists(fun (x, y) -> (x > num) && (y < num)))     
      
let getResultAsString (pairs : (int * int) list) numsInBetweenPairs =
      let pairOutput = pairs |> List.sortBy(fun (f, _) -> -f) |> List.map(fun (f,s) -> printf "%d %d" f s) |> (fun s -> String.Join(" ", s)) 
      let betweenNumsOutput = numsInBetweenPairs |> List.map(fun n -> printfn "%d %d" n) |> (fun s -> String.Join(" ", s))       
      sprintf "%s %s" pairOutput betweenNumsOutput      

let solve line1 line2 = 
     let _, xs = getInputFromLines line1 line2
     let pairs, _ = xs |> Seq.toList |> getPairsWithSmallestDiff
     let numsBetweenPairs = pairs |> getNumberBetweenPairs     
     getResultAsString pairs numsBetweenPairs
      



