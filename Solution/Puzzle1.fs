﻿namespace FSharp.Puzzles.Solutions

module Puzzle1 = 
    let TakeNthElementFromTail(xs, n) : Option<'a> =       
        if ((List.length xs) >= n) && (n > 0) then
            Some(List.nth (List.rev xs) (n - 1))             
        else
            None
         
