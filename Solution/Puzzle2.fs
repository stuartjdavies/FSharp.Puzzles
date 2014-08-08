namespace FSharp.Puzzles.Solutions

type TriangleType =
        | Scalene 
        | Isosceles
        | Equilateral
        | Error   

module Puzzle2 = 
    let private IsEquilateral(len1, len2, len3) = (len1=len2) && (len3 = len2) 
    let private IsIsosceles(len1, len2, len3) = (len1 = len3 && len1 <> len2) || 
                                                (len2 = len3 && len1 <> len2) || 
                                                (len1 = len2 && len1 <> len3)
    let private IsScalene(len1, len2, len3) = (len1 <> len2) && (len1 <> len3) &&
                                              (len2 <> len3)                                        
    let GetTriangleType(lengths) =                               
        match(lengths) with
        | [] -> Error
        | [x;y;z] when (x > 0.0F && y > 0.0F && z > 0.0F) -> 
                  if IsEquilateral(x, y, z) then
                    Equilateral
                  else if IsIsosceles(x, y, z) then
                    Isosceles
                  else if IsScalene(x, y, z) then
                    Scalene
                  else
                    Error
        | _ -> Error  
