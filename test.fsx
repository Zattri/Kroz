type Command =
    |Use
    |Move
    |Help
    |Hint
    |Rotate
    |Go
    |Search
    |Look
    |Open
    |Close
    |Push
    |Pull


type Item = 
  {
    id:int;
    name:string; 
  }

type WorldObject = 
  {
    id:int;
    name:string;
    state:int;
  }

type Location = 
  {
    name:string;
    state:int;  
    items: Set<Item>;
    objects: Set<WorldObject>;
  }


let obj = {id=1; name="Button"; state=0}
let obj2 = {id=2; name="Lever"; state=1}


let checkwObjectState (wObject:WorldObject) checkState = 
  checkState = wObject.state

let incrementwObjectState (wObject:WorldObject) = 
  {wObject with state = wObject.state + 1}


// Main program

// Test to check state
checkwObjectState obj 0

// Test to check state after update
let newObj = incrementwObjectState obj
printfn "%A" newObj
checkwObjectState newObj 0

type InteractionTuple = Command * WorldObject * int
type ResultTuple = string * int

let dictTest = dict[
    (Push, obj, 0),("You have pushed the button", 2);
    (Pull, obj2, 1),("You have pulled the lever", 0);
]

let keys = dictTest.Keys

Seq.iter (fun key -> printfn "%A" (dictTest.Item(key))) (dictTest.Keys)

let testLoc = {
    name = "Pyramid";
    state = 0;
    items = Set.ofList [{id=0; name="Dagger"}; {id=1; name="Sword"}];
    objects = Set.ofList [{id= 1; name = "Pillar"; state = 0;}; {id = 2; name = "Door"; state = 1}]
}



printfn "%A" testLoc