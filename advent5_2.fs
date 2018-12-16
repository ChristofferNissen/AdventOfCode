open System 
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Text

let readLines filePath = System.IO.File.ReadLines(filePath).ToList()

// logic for removing chars from char list
let rec react acc list = 
  match list with 
  | s :: c :: xs -> 
    
    let r1 = Char.IsUpper s
    let r2 = Char.IsUpper c
    let r = r1 <> r2

    match (((Char.ToLower s) = (Char.ToLower c)) && r) with
    | true  -> react acc xs 
    | false -> react ([s]@acc) ([c]@xs)

  | [s] -> ([s]@acc)
  | _ -> acc

// converts char list to string
let rec charsToString (v: char list) (acc:string) = 
    match v with 
    | x :: xs -> charsToString xs (x.ToString() + acc)
    | [] -> acc

let rec runToCompletion list = 
  let x = react List.empty list
  let xx = react List.empty x
  match x.Length > xx.Length with
  | true  -> runToCompletion xx
  | false -> xx

let fullyReactedString list =  charsToString (runToCompletion list) String.Empty

// logic for removing chars from char list [SPECIFIC CHAR]
let reactWithChar char array = 
  let rec react char acc list = 
    match list with 
    | s :: c :: xs -> 
      
      let bool1 = (Char.ToLower s) = char 
      let bool2 = (Char.ToLower c) = char
      let r1 = Char.IsUpper s
      let r2 = Char.IsUpper c
      let r = r1 <> r2 // wether one of them is true

      match ((((Char.ToLower s) = (Char.ToLower c)) && r) || bool1 || bool2) with
      | true -> 
        match bool1 with
        | true -> react char acc ([c]@xs)
        | false -> 
          match bool2 with
          | true -> react char acc ([s]@xs)
          | false -> react char acc xs 

      | false -> react char ([s]@acc) ([c]@xs)

    | [s] -> fullyReactedString ([s]@acc)
    | _ -> fullyReactedString acc

  // fullyReactedString but with char
  // ugly while loop. Runs while the string length is decreasing
  let mutable x = react char List.empty (List.ofArray array)
  let mutable xx = react char List.empty  (List.ofArray (x.ToCharArray()))
  while (x.Length > xx.Length) do
    //printfn "RUNNING ONE MORE TIME"
    x <- xx
    xx <- react char List.empty (List.ofArray (xx.ToCharArray())) 

  (charsToString (List.ofArray (xx.ToCharArray())) String.Empty)

[<EntryPoint>]
let main argv =
  let lines = readLines "/Users/cn/Desktop/input6.txt"
  let s = lines.First()
  let array = s.ToCharArray()
  
  for i in 'a'..'z' do
    let x = reactWithChar i array
    printfn "%c %d" i x.Length

  0