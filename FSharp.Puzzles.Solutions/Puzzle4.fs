module FSharp.Puzzles.Solutions.Puzzle4

let solve(a,b,c,d) =
        let rec aux (x,y) =                 
                 if (x = c && y = d) then true
                 else if (x > c || y > d) then false
                 else 
                    if (aux ((x + y, y)) = true) then true else aux ((x, x + y))
        aux (a,b)


        




