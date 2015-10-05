module FSharp.Puzzles.Puzzle7Solution2

// Using arrays can speed up things  ...
let ($) item (items : _ array) = Array.append ([| item |]) items

let rec getElimIndex counter pointer length =                                                                                
        if counter = 1 then pointer 
        else 
            if pointer + 1 >= length then getElimIndex (counter - 1) 0 length  
            else getElimIndex (counter - 1) (pointer + 1) length                                                                                                

let removeItem (n : int) (items : _ array) = 
        let index = getElimIndex n 0 items.Length
                                   
        if (items.Length <= 1) then
            items.[ index ], Array.empty 
        else 
            if index = 0 then items.[ index ], items.[ (index + 1) .. ] 
            else items.[ index ], Array.append items.[ (index + 1) .. ] items.[ 0 .. index - 1 ]                  
                
let getOrderOfRemovals n (items : _ array) =
        let rec aux acc n (xs : _ array) = 
                    if xs.Length < 1 then acc                                                                 
                    else                                 
                        let eliminated, lst = removeItem n xs                                     
                        aux (eliminated $ acc) n lst       
        if (items.Length < 1 || n < 1) then [||] else aux [||] n items  

