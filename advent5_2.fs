open System 
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Text

let readLines filePath = System.IO.File.ReadLines(filePath).ToList()

let rec react acc list = 
  match list with 
  | s :: c :: xs -> 
    let r1 = Char.IsUpper s
    let r2 = Char.IsUpper c
    let r = r1 <> r2

    if (((Char.ToLower s) = (Char.ToLower c)) && r)
    then
      //printfn "FOUND MATCH %c %c" s c
      react acc xs 
    else
      react ([s]@acc) ([c]@xs)

  | [s] -> ([s]@acc)
  | _ -> acc

let rec charsToString (v: char list) (acc:string) = 
    match v with 
    | x :: xs -> charsToString xs (x.ToString() + acc)
    | [] -> acc
    
let fullyReact list =  
  // ugly while loop. Runs while the string length is decreasing
  let mutable x = react List.empty list
  let mutable xx = react List.empty x
  while (x.Length > xx.Length) do
    //printfn "STARTING OVER"
    x <- xx
    xx <- react List.empty xx 

  charsToString xx String.Empty

let reactWithChar char array = 
  let rec react char acc list = 
    match list with 
    | s :: c :: xs -> 
      let bool1 = (Char.ToLower s) = char 
      let bool2 = (Char.ToLower c) = char
      let r1 = Char.IsUpper s
      let r2 = Char.IsUpper c
      // wether one of them is true
      let r = r1 <> r2

      if ((((Char.ToLower s) = (Char.ToLower c)) && r) || bool1 || bool2)
      then
        if bool1 then react char acc ([c]@xs)
        elif bool2 then react char acc ([s]@xs)
        else 
          //printfn "FOUND MATCH %c %c" s c
          react char acc xs 
      else
        react char ([s]@acc) ([c]@xs)
    | [s] -> fullyReact ([s]@acc)
    | _ -> fullyReact acc

  // fullyreact but with char
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