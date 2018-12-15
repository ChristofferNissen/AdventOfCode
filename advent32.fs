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

  let mutable grid = Array2D.init<string> 2000 2000 (fun row col -> sprintf "%d" 0)

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
    let (left, top) = ((tmp.[0]).Trim ' ',tmp.[1])
    let (wide, tall) = ((tmp2.[0]).Trim ' ', tmp2.[1])
    
    //let (left, top ) = position.Split ','
    //let (x,y) = dimensions.Split 'x'

    //let x = grid.[int left,int top]
    
    // fill in array 
    //printfn "Left: %s Top: %s Wide: %s Tall: %s" left top wide tall
    fillArray grid ((x.[0]).Trim ' ') (int left) (int top) (int wide) (int tall) 
  (*
  // loop over array, count X
  let result = 
    let mutable count = 0
    for i = 0 to 1999 do
      for y = 0 to 1999 do
        if grid.[i,y] = "X"
        then count <- (count+1)
        
    count

  printfn "Number of overlapping squares: %d" result
  *)
  // Check integrity of squares
  for s in lines do
    // extract info
    let x = s.Split '@'
    let data = (x.[1]).Split ':'
    let position = (data.[0])
    let tmp = position.Split ','
    let dimensions = (data.[1])
    let tmp2 = dimensions.Split 'x'
    let (left, top) = ((tmp.[0]).Trim ' ',tmp.[1])
    let (wide, tall) = ((tmp2.[0]).Trim ' ', tmp2.[1])

    let result = checkArray grid ((x.[0]).Trim ' ') (int left) (int top) (int wide) (int tall) 
    if result then printfn "Result: %b Id: %s" result ((x.[0]).Trim ' ') 



(*
  let initiateBoard = 
    for r = 0 to Array2D.length1 grid - 1 do
      for c = 0 to Array2D.length2 grid - 1 do
        printfn "%A " grid.[r, c]
*)
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