open System 
open System.Collections
open System.Collections.Generic
open System.Collections.Generic
open System.Linq
open System.Collections.Generic

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
  let lines = readLines "/Users/cn/Desktop/input6.txt"
  let mutable list = List.empty

  for s in lines do
    // extract info
    let x = s.Split ']'
    let date = x.[0].Substring 1
    //printfn "%s" (date.ToString())
    //let d = DateTime.ParseExact(date, "yyyy-MM-dd hh:mm", null)
    let d = DateTime.Parse(date)
    //printfn "%s" (d.ToString())
    //printfn "%s" (d.ToString "yyyy/MM/dd mm:ss")
    let c = x.[1].Trim ' '

    list <- List.append list [(d,c)]

  // sort list
  list <- List.sortBy (fun (date, command) -> date ) list
  //for (d,c) in list do printfn "%s %s" (d.ToString "yyyy/MM/dd hh:mm:ss") c

  // count minuts asleep

  //for i = 0 to (list.Length-1) do
  let mutable map = Map.empty
  let mutable currentGuard = 0
  let mutable sleepStart = DateTime.Now
  for (d,c) in list do 
    match c with
    | "falls asleep" -> 
      sleepStart <- d
      //printfn "Asleep %s Guard: %d" (d.ToString()) currentGuard
    | "wakes up" -> 
      //printfn "%d %s" currentGuard (sleepStart.ToString())
      let diff = d - sleepStart
      //printfn "Guard %d wakes after %f" currentGuard diff.TotalSeconds
      if (Map.containsKey currentGuard map)
      then
        let (x,y) = Map.find currentGuard map
        let m = x @ [(sleepStart.Minute, diff.TotalMinutes)]
        //let m = ma.Append (int) sleepStart.Minute 
        map <- Map.add (currentGuard) (m, (diff.TotalSeconds + y)) map
      else 
        let i = [(sleepStart.Minute, diff.TotalMinutes)]
        map <- Map.add (currentGuard) (i, diff.TotalSeconds) map

    | _ ->
      //printfn "%s" c 
      let x = c.Split 'b'
      let guard = x.[0].Trim ' '
      let guardNr = (guard.Split '#').[1].Trim ' ' 
      //printfn "%d" (int guardNr)
      currentGuard <- int guardNr
      // falls asleep
      //printfn "%d" time.Seconds
      // insert time between falls aleep and wakes up
      //printfn "%s" guardNr

  let mutable maxSleep = (0,0)    
  let mutable newMap = Map.empty
  for entry in map do 
    let x = entry.Value
    let (y,z) = x
    let (guard, sleep) = maxSleep
    //printfn "Evaluating %d %f CURRENT MAX: %d %f" entry.Key z guard sleep
    if( (int z) > sleep) 
    then 
      //printfn "new Max Sleep %d %f" entry.Key z
      maxSleep <- (entry.Key, (int z))
    for (tmp, tmp2) in y do
      //printfn "%d %d" tmp (int tmp2)
      // count in map
      for i in tmp .. (tmp + (int tmp2))-1 do
        //printfn "%d %d %f %d" i tmp tmp2 (tmp + (int tmp2))
        if (newMap.ContainsKey (entry.Key, i))
        then
          let tp = newMap.Item ( entry.Key, i)
          newMap <- newMap.Add ((entry.Key, i), (tp+1))
        else
          newMap <- newMap.Add ((entry.Key, i), 1)

  let (xx,y) = maxSleep    
  printf "%d %d\n" xx y

  //for entry in newMap do 
    //let (x,y) = entry.Key
    //if( x = 2389) then
    // printfn "x: %d y: %d z: %d" x y entry.Value

    // extract info from map
  let mutable max = ((0,0),0)
  let mutable max2 = ((0,0),0)
  for entry in newMap do
    let (x,y) = entry.Key
    //printfn "%d %d %d" x y entry.Value
    let (k1,v1) = max2
      
    if(entry.Value > v1) 
    then 
      //printfn "Setting new max %d %d %d" x y entry.Value 
      max2 <- ((x,y), entry.Value)

    if xx = x then
      let (k,v) = max
      
      if(entry.Value > v) 
      then 
        //printfn "Setting new max %d %d %d" x y entry.Value 
        max <- ((x,y), entry.Value)
        
  let ((x,z),y) = max
  printfn "Guard that sleeps the most: Gurad: %d, Minut: %d Times: %d" x z y
  printfn "Result: (%d*%d) = %d" x z (x*z) 
  let ((x1,z1),y1) = max2
  printfn "Guard that sleeps the most at a specifc minut: Guard: %d, Minut: %d Times: %d" x1 z1 y1
  printfn "Result: (%d*%d) = %d" x1 z1 (x1*z1) 
      //printfn "K: %d V: %d F: %f " entry.Key tmp z

(*
    let data = (x.[1]).Split ':'
    ls

    let position = (data.[0])
    let tmp = position.Split ','
    let dimensions = (data.[1])
    let tmp2 = dimensions.Split 'x'
    let (left, top) = ((tmp.[0]).Trim ' ',tmp.[1])
    let (wide, tall) = ((tmp2.[0]).Trim ' ', tmp2.[1])
 *)  


  stopWatch.Stop()

  
  0