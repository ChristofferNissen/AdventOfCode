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

[<EntryPoint>]
let main argv =
  let stopWatch = System.Diagnostics.Stopwatch.StartNew()
  let lines = readLines "/Users/cn/Desktop/input3.txt"

  for i = 1 to (lines.Count-1) do
    for y = (1+i) to (lines.Count-1) do 
      let x = compareStrings (lines.Item i) (lines.Item y)
      if x = 1 
      then 
        // Match found where only one differ. 
        printfn "Found match: %s %s" (lines.Item i) (lines.Item y)
        printfn "%s" (compareStrings2 (lines.Item i) (lines.Item y))

  // compare current line to all others, if no difference of one 1 is found, go on to next line

  //let mutable numberOfTwos = 0
  //let mutable numberOfThrees = 0
  //for s in lines do 
    //let map = getMap s
    //for entry in map do  printfn "%c %d" entry.Key entry.Value

    // single loop count both 2's and 3's 
    //let (tmp2, tmp3) =  Map.fold (fun ((state:int),(state1:int)) (key:char) (value:int) -> 
      //let (tmpState,tmpState1) = if (value = 2) then ((state+1), state1) else (state,state1) 
      //if (value = 3) then ((tmpState), tmpState1+1) else (tmpState, tmpState1) ) (0,0) map
    
    //numberOfTwos <- if tmp2 > 0 then numberOfTwos + 1 else numberOfTwos
    //numberOfThrees <- if tmp3 > 0 then numberOfThrees + 1 else numberOfThrees
   
  //printfn "%d" numberOfTwos
  //printfn "%d" numberOfThrees
  stopWatch.Stop()
  //printfn "time: %f ms" stopWatch.Elapsed.TotalMilliseconds
  //printfn "checksum: %d" (numberOfTwos * numberOfThrees)
  
  0open System 
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

[<EntryPoint>]
let main argv =
  let stopWatch = System.Diagnostics.Stopwatch.StartNew()
  let lines = readLines "/Users/cn/Desktop/input4.txt"

  // loop over all lines paiting squares in array
    // extract info 

  for s in lines do
    // extract info
    let x = s.Split '@'
    let data = (x.[1]).Split ':'
    let position = (data.[0])
    let tmp = position.Split ','
    let dimensions = (data.[1])
    let tmp2 = dimensions.Split 'x'
    let (left, top) = (tmp.[0],tmp.[1])
    let (x,y) = (tmp2.[0], tmp2.[1])

    //let (left, top ) = position.Split ','
    //let (x,y) = dimensions.Split 'x'

    printfn "Left: %s Top: %s x: %s y: %s" left top x y



(*
  for i = 1 to (lines.Count-1) do
    for y = (1+i) to (lines.Count-1) do 
      let x = compareStrings (lines.Item i) (lines.Item y)
      if x = 1 
      then 
        // Match found where only one differ. 
        printfn "Found match: %s %s" (lines.Item i) (lines.Item y)
        printfn "%s" (compareStrings2 (lines.Item i) (lines.Item y))
*)
  // compare current line to all others, if no difference of one 1 is found, go on to next line

  //let mutable numberOfTwos = 0
  //let mutable numberOfThrees = 0
  //for s in lines do 
    //let map = getMap s
    //for entry in map do  printfn "%c %d" entry.Key entry.Value

    // single loop count both 2's and 3's 
    //let (tmp2, tmp3) =  Map.fold (fun ((state:int),(state1:int)) (key:char) (value:int) -> 
      //let (tmpState,tmpState1) = if (value = 2) then ((state+1), state1) else (state,state1) 
      //if (value = 3) then ((tmpState), tmpState1+1) else (tmpState, tmpState1) ) (0,0) map
    
    //numberOfTwos <- if tmp2 > 0 then numberOfTwos + 1 else numberOfTwos
    //numberOfThrees <- if tmp3 > 0 then numberOfThrees + 1 else numberOfThrees
   
  //printfn "%d" numberOfTwos
  //printfn "%d" numberOfThrees
  stopWatch.Stop()
  //printfn "time: %f ms" stopWatch.Elapsed.TotalMilliseconds
  //printfn "checksum: %d" (numberOfTwos * numberOfThrees)
  
  0