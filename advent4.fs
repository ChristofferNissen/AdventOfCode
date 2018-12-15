open System 
open System.Collections
open System.Collections.Generic
open System.Collections.Generic
open System.Linq

let readLines filePath = 
  System.IO.File.ReadLines(filePath).ToList()

let getMap (s:string) : Map<char, int> = 
  let chararray = s.ToCharArray()
  let mutable map = Map.empty
  for c in chararray do 
      if Map.containsKey c map 
      then
        let value = Map.find c map
        map <- Map.add c (value + 1) map
      else
        map <- Map.add c 1 map
  map

let rec compare acc (s1:list<char>) (s2:list<char>) =
  let x = match (s1, s2) with
          | (s :: sx, ss :: ssx) when s = ss -> compare acc sx ssx
          | (s :: sx, ss :: ssx) -> compare(acc+1) sx ssx
          | (_,_) -> acc
  x

let rec compare2 (acc:string) (s1:list<char>) (s2:list<char>) =
  let x = match (s1, s2) with
          | (s :: sx, ss :: ssx) when s = ss -> compare2 (acc + string s) sx ssx
          | (s :: sx, ss :: ssx) -> compare2(acc) sx ssx
          | (_,_) -> acc
  x

let compareStrings (s1:string) (s2:string) = 
  (compare 0 (Seq.toList s1) (Seq.toList s2))

let compareStrings2 (s1:string) (s2:string) = 
  (compare2 (String.Empty) (Seq.toList s1) (Seq.toList s2))

let fillArray (a:string[,]) ID left top wide tall = 
  // start at left, top and work down to left+wide and top + tall
  for i = left to (left+wide-1) do
    for y = top to (top+tall-1) do
      let tmp = (a.[i,y])
      //printfn "%s" tmp
      if (tmp.Contains "#")
      then
        a.[i,y] <- (a.[i,y]).Replace(tmp, "X")
        //printfn "%s" x
      elif (tmp.Contains "X")
      then 
        a.[i,y] <- a.[i,y] 
        //printfn "%s" "X" 
      else 
        a.[i,y] <- (a.[i,y]).Replace(tmp,ID)
        //printfn "%s" x

let checkArray (a:string[,]) (ID:string) left top wide tall = 
  let mutable result = true
  // start at left, top and work down to left+wide and top + tall
  for i = left to (left+wide-1) do
    for y = top to (top+tall-1) do
      let tmp = (a.[i,y])
      //printfn "%s" tmp
      let x = tmp.Contains ID
      if (x)
      then
        if result 
        then result <- true
      else 
        result <- false
  result
         
[<EntryPoint>]
let main argv =
  let stopWatch = System.Diagnostics.Stopwatch.StartNew()
  let lines = readLines "/Users/cn/Desktop/input4.txt"

  for s in lines do
    // extract info
    let x = s.Split ']'
    let date = x.[0].Remove 0
    let d = DateTime.ParseExact("20100503", "yyyyMMdd", null)
    printfn "%s" (d.ToString "yyyy/MM/dd")
(*
    let data = (x.[1]).Split ':'
    let position = (data.[0])
    let tmp = position.Split ','
    let dimensions = (data.[1])
    let tmp2 = dimensions.Split 'x'
    let (left, top) = ((tmp.[0]).Trim ' ',tmp.[1])
    let (wide, tall) = ((tmp2.[0]).Trim ' ', tmp2.[1])
 *)  


  stopWatch.Stop()

  
  0