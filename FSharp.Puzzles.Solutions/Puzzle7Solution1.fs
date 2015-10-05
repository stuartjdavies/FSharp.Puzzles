module FSharp.Puzzles.Puzzle7Solution1

let removeItem n lst =        
        let rec aux acc count xs  =                     
                        match xs with                                                                                                            
                        | [] -> if count >= 1 then aux [] count lst else failwith("Count must be greater than 1") // Else Should not be executed
                        | h::t -> if count = 1 then h, t @ (List.rev acc) else aux (h::acc) (count - 1) t 
            
        aux [] n lst                            

let getOrderOfRemovals n lst =
        let rec aux acc n xs = 
                        if xs = [] then acc 
                        else                                 
                            let eliminated, lst = removeItem n xs // removeElem n xs                                    
                            aux (eliminated::acc) n lst
        if n < 1 then [] else aux [] n lst  

