open System 
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Text
let readLines filePath = System.IO.File.ReadLines(filePath).ToList()

let rec run (s: string list) acc : string list = 
  printfn "%d" s.Length
  match s with
    | s :: c :: xs when ((s = c.ToUpper()) || (s.ToUpper() = c)) -> run xs acc
    | s :: c :: xs -> run xs ([s;c;]@acc)
    | [s] -> run [] ([s]@acc)
    | [] -> acc

[<EntryPoint>]
let main argv =
  let lines = readLines "/Users/cn/Desktop/input6.txt"
  let s = lines.First()
  let array = s.ToCharArray()

  let rec react acc list = 
    match list with 
          | s :: c :: xs -> 
            //printfn "%c %c" s c
            //printfn "%c %c" (Char.ToLower s) (Char.ToLower c)
            let r1 = Char.IsUpper s
            let r2 = Char.IsUpper c
            let r = r1 <> r2

            //printfn "s: %c c: %c" s c
            //printfn "r1: %b r2: %b r: %b" r1 r2 r

            if (( (Char.ToLower s) = (Char.ToLower c)) && r)
            then
              printfn "FOUND MATCH %c %c" s c
              react acc xs 
            else
              react ([s]@acc) ([c]@xs)

          | [s] -> ([s]@acc)
          | _ -> acc



  // List.ofArray array
  let mutable x = react List.empty (List.ofArray array)
  let mutable xx = react List.empty  x

  let mutable xxx = react List.empty  xx

  while (x.Length > xx.Length) do
    //printfn "STARTING OVER"
    let xxx = react List.empty xx
    printfn "x: %d xx: %d xxx: %d" x.Length xx.Length xxx.Length
    x <- xx
    xx <- xxx
  
  let mutable string = ""
  for e in xx do
    string <- string + e.ToString()

  printfn "%d" xx.Length
  printfn "%d" xxx.Length
  printfn "%s" string
  //printfn "%d" lines.Count

  0
