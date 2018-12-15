open System 
open System.Collections
open System.Collections.Generic

let readLines filePath = System.IO.File.ReadLines(filePath);;

let rec run (lines: list<string>) (l : list<string>) (acc:int) (set:Set<int>) : int = 
  match lines with
    | s :: xs ->
      let value = int s
      let value2 = acc + value
      let x = Set.exists (fun x -> x = value2) set
      
      //printfn "%b\n" x
      //printfn "Value 2: %d\n" value2
      //printfn "Set size: %d\n" set.Count
      
      if x 
      then value2 
      else run (xs) l value2 (set.Add value2)

    | _ -> 
      printfn "Starting over with lines length %d acc %d set count %d" l.Length acc set.Count
      run l l acc (set)

[<EntryPoint>]
let main argv =
  let lines = readLines "/Users/cn/Desktop/input.txt"
  
  let mutable set = Set.empty

  let mutable result = (run (List.ofSeq lines) (List.ofSeq lines) 0 set)

  //while (result = 0) do result <- (run (List.ofSeq lines) 0 set)
  
  printfn "%i" result
  0
