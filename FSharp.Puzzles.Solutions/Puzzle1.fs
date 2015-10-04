module FSharp.Puzzles.Solutions.Puzzle1

let takeNthElementFromTail(xs, n) : Option<'a> =       
       if ((List.length xs) >= n) && (n > 0) then Some(List.nth (List.rev xs) (n - 1)) else None
         
