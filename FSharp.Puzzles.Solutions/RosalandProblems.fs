module FSharp.Puzzles.Solutions.Rosaland

open System
open System.Linq
open System.Text.RegularExpressions

type DnaString = string
type RnaString = string

//
//  Title: Counting DNA Nucleotides
//  Detals: http://rosalind.info/problems/dna/
//
let solveProblemWithId_DNA (s : DnaString) = (" ", [| 'A'; 'C'; 'G'; 'T' |] |> Array.map(fun k -> s.Count(fun sym -> sym = k))) |> String.Join
                    
//
// Title Transcribing DNA into RNA
// Details: http://rosalind.info/problems/rna/
// 
let solveProblemWithId_RNA (s : DnaString) = new DnaString [| for k in s -> if k = 'T' then 'U' else k |]

//
// Title: Complementing a Strand of DNA 
// Details: http://rosalind.info/problems/revc/
// 
let solveProblemWithId_REVC (s : DnaString) = 
           let revComplement c = match c with 
                                 | 'A' -> 'T'
                                 | 'T' -> 'A'
                                 | 'C' -> 'G'
                                 | 'G' -> 'C'
                                 | _ -> c
           s |> Seq.toArray |> Array.rev |> Array.map revComplement |> (fun xs -> new string(xs))
 
//
// Title: Rabbits and Recurrence Relations
// Details: http://rosalind.info/problems/fib/
// 
let solveProblemWithId_FIB (n : int) (k : int) = String.Empty
    
//
// Title: Computing GC Content      
// Details: http://rosalind.info/problems/gc/
// TODO: Absolute Error
//
let sovleProblemWithId_GC (ss : DnaString array)=       
        let matches = ss |> Seq.fold(fun acc s -> acc + (float (s.Count(fun item -> item='C' || item='G')))) 0.0
        let totalItems = ss |> Seq.fold(fun acc s -> acc + (float (s.Length))) 0.0
        (matches / totalItems) * 100.00
              
// 
// Title: Counting Point mutations
// Details: http://rosalind.info/problems/hamm/
//
let solveProblemWithId_HAMM (s : DnaString) (t : DnaString) = 
           (s, t) ||> Seq.map2 (fun sItem tItem -> if sItem <> tItem then 1 else 0) |> Seq.sum


let solveProblemWithId_IPRB = String.Empty

//
// Title: Translating RNA into Protein
// Details: http://rosalind.info/problems/prot/                    
// 
let solveProblemWithId_PROT (s : RnaString) = 
          let rnaCondonTable = [| "UUU","F";"CUU","L";"AUU","I";"GUU","V"
                                  "UUC","F";"CUC","L";"AUC","I";"GUC","V"
                                  "UUA","L";"CUA","L";"AUA","I";"GUA","V"
                                  "UUG","L";"CUG","L";"AUG","M";"GUG","V"
                                  "UCU","S";"CCU","P";"ACU","T";"GCU","A"
                                  "UCC","S";"CCC","P";"ACC","T";"GCC","A"
                                  "UCA","S";"CCA","P";"ACA","T";"GCA","A"
                                  "UCG","S";"CCG","P";"ACG","T";"GCG","A"
                                  "UAU","Y";"CAU","H";"AAU","N";"GAU","D"
                                  "UAC","Y";"CAC","H";"AAC","N";"GAC","D"
                                  "UAA","Stop";"CAA","Q";"AAA","K";"GAA","E"
                                  "UAG","Stop";"CAG","Q";"AAG","K";"GAG","E"
                                  "UGU","C";"CGU","R";"AGU","S";"GGU","G"
                                  "UGC","C";"CGC","R";"AGC","S";"GGC","G"
                                  "UGA","Stop";"CGA","R";"AGA","R";"GGA","G"
                                  "UGG","W";"CGG","R";"AGG","R";"GGG","G"|]   
          let rec aux acc (items : string) =             
                let k, v = rnaCondonTable |> Array.find(fun (k, _) -> k=items.[0..2])                
                if (items.Length > 3) then
                    aux (v.[0]::acc) items.[3 .. ]
                else
                    (v.[0]::acc) 
          aux List.Empty s |> List.rev |> List.toArray |> (fun items -> new string (items))          

// 
// Title: Finding a Motif in DNA
// Details: http://rosalind.info/problems/subs/                
//
let solveProblemWithId_SUBS (s : DnaString) (t : DnaString) = 
        let rec aux acc (index : int) =
            let index = s.IndexOf(t, index)
            if index < 0 then 
              acc 
            else 
              aux ((index + 1)::acc) (index + 1) 
        aux [] 0 |> List.rev                     
           
let solveProblemWithId_CONS (s : RnaString) = String.Empty
        
        
//
// Title: Finding a Shared Motif 
// Details: http://rosalind.info/problems/lcsm/
//
let solveProblemWithId_LCSM (s : DnaString) = 
           Regex(@"(.+)\1", RegexOptions.IgnoreCase).Match(s) 

           //let rec findMatch startIndex endIndex = 
           //             let subStr = s.[startIndex .. endIndex ]
           //             if s.Contains(subStr) = true then subStr
           //             else 
           //                 if startIndex = endIndex then String.Empty else findMatch startIndex (endIndex - 1)
           //[| 0 .. (s.Length - 1) |] |> Array.map(fun i -> findMatch i (s.Length - 1))                  
                               
solveProblemWithId_LCSM "aa12bbbbb2bbbbb4aaa225" |> (fun m -> m.Groups.[2])                               
                           
          