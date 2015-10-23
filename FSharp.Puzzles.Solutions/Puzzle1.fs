module FSharp.Puzzles.Solutions.Puzzle1

let takeNthElementFromTail (xs : 'a list) n = if (xs.Length >= n) && (n > 0) then Some xs.[ xs.Length - n ] else None
         
