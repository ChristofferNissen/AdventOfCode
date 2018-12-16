open System 
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Text
open System.Collections.Generic

let readLines filePath = System.IO.File.ReadLines(filePath).ToList()

type Node(s) = 
  let mutable Childrens  = List.empty
  let mutable Parents = List.empty
  member this.Step = s

  interface IComparable with
    member this.CompareTo obj = 
      match obj with
      | :? Node as x -> (this.Step :> IComparable<_>).CompareTo x.Step

  member this.Children 
    with get () = Childrens
    and set (value) = Childrens <- value

  member this.Parent
    with get () = Parents
    and set (value) = Parents <- value

[<EntryPoint>]
let main argv =
  let lines = readLines "/Users/cn/Desktop/input7.txt"
  let mutable (allSteps: Node list) = []
  let mutable (finalSteps: Node list) = []
  let mutable map = Map.empty
  for i in 'A'..'Z' do map <- Map.add (i.ToString()) [] map
  for s in lines do
    //printfn "%s" s
    let parent = (s.Split ' ').[1]
    let child = (s.Split ' ').[7]

    let nodeParent = Node(parent)
    let nodeChild = Node(child)

    if not (allSteps.Contains(nodeParent))
    then allSteps <- allSteps@[nodeParent]

    if not (allSteps.Contains(nodeChild))
    then allSteps <- allSteps@[nodeChild]

    let parentRelationship = allSteps.Where( fun o -> String.Equals(o.Step, parent)).First()
    //printfn "PARENT RELATIONSHIP: %s" parentRelationsship.Step

    if not (parentRelationship.Children.Contains(nodeChild)) 
    then
      let x = parentRelationship.Children
      let xxx = x.ToList()
      xxx.Add nodeChild
      parentRelationship.Children <- List.ofArray (xxx.ToArray())
      //printfn "1: %d %d" x.Length xxx.Count 
    else 
      let x = parentRelationship.Children
      let xxx = x.ToList()
      xxx.Remove nodeChild |> ignore
      //printfn "2: %d %d" x.Length xxx.Count 

    let childRelationship = allSteps.Where( fun o -> String.Equals(o.Step, child)).First()

    if not (childRelationship.Parent.Contains(nodeParent)) 
    then
      let x = childRelationship.Parent
      let xxx = x.ToList()
      xxx.Add nodeParent
      childRelationship.Parent <- List.ofArray (xxx.ToArray())
      //printfn "3: %d %d" x.Length xxx.Count 
    else 
      let x = childRelationship.Parent
      let xxx = x.ToList()
      xxx.Remove nodeParent |> ignore
      //printfn "4: %d %d" x.Length xxx.Count 

  while allSteps.Length > 0 do
    let potentialSteps = allSteps.Where(fun x -> not (x.Parent.Length > 0) ).ToList()
    potentialSteps.Sort()
    let potentialStep = potentialSteps.First()
    (*
    printfn "Size of possible choices %d" potentialSteps.Count
    for ele in potentialSteps do
      printfn "Node: %s Parents: %d Childrens: %d" ele.Step (ele.Parent.ToList().Count) (ele.Children.ToList().Count)
*)
    finalSteps <- [potentialStep]@finalSteps

    //let tmp = allSteps.ToList()
    //tmp.Remove potentialStep |> ignore

    let tmp = allSteps.Where(fun o -> not (String.Equals(o.Step, potentialStep.Step)))    

    let result = allSteps.Where(fun x -> x.Parent.Contains(potentialStep)).ToList()
    result.ForEach(fun y -> 
      let x = y.Parent.ToList()
      //printfn "XX %d" x.Count
      x.Remove potentialStep |> ignore // remove from mutabale list
      //printfn "XXX %d" x.Count
      y.Parent <- List.ofArray (x.ToArray())
      //printfn "XXXX %d" (y.Parent.ToList().Count)
    )
    
    //printfn "%d" result.Count
    allSteps <- (List.ofArray (tmp.ToArray()))

  // print result
  let mutable string = ""
  for el in finalSteps do
    string <- el.Step + string

  printfn "%s" string
    
  0