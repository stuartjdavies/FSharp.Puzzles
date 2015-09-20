namespace FSharp.Puzzles.Solutions

type TriangleType =
        | Scalene 
        | Isosceles
        | Equilateral
        | Error   

module Puzzle2 = 
    let private isEquilateral len1 len2 len3 = (len1=len2) && (len3 = len2) 
    let private isIsosceles len1 len2 len3 = (len1 = len3 && len1 <> len2) || 
                                             (len2 = len3 && len1 <> len2) || 
                                             (len1 = len2 && len1 <> len3)
    //let private isScalene len1 len2 len3 = (len1 <> len2) && (len1 <> len3) && (len2 <> len3)                                        
    let getTriangleType(len1,len2,len3) =                               
          if isEquilateral len1 len2 len3 then
            Equilateral
          else if isIsosceles len1 len2 len3 then
            Isosceles
          else 
            Scalene
         
        
