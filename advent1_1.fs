open System 
open System.Collections
open System.Collections.Generic

let readLines filePath = System.IO.File.ReadLines(filePath);;

let rec run (lines: list<string>) acc : int = 
  match lines with
    | s :: xs -> 
      run (xs) (acc + int s)
    | [] -> acc

[<EntryPoint>]
let main argv =
  let lines = readLines "/Users/cn/Desktop/input.txt"
  printfn "%i" (run (List.ofSeq lines) 0)
  0