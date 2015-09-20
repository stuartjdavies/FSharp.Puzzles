namespace FSharp.Puzzles.Solutions

open System

module Puzzle3 =
    let private toList(s : string) = s.ToCharArray() |> Array.toList          
    let reverseWordsInString(str) =       
        let rec aux(acc, word, cs) =
            match (cs) with
            | [] -> List.rev(word) @ acc                                       
            | x::xs -> if (x = ' ') then
                         if (List.length word) > 0 then
                           aux(x::((List.rev word) @ acc), [], xs)
                         else
                           aux(x::acc, [], xs)
                       else 
                           aux(acc, x::word, xs) 
        let xs = aux([], [], toList(str)) |> List.rev |> List.toArray 
        new String(xs)