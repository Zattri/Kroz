type Item = 
  {
    id:int;
    name:string; 
  }

type WorldObject = 
  {
    name:string;
    id:int;
    state:int;
  }

(*
type Location = 
  {
    name:string;
    state:int;  
    items: (Item); // Needs to be a set of items
    objects: (WorldObject);
  }
*)

let interactionDict = dict[
  ("Test", 1), ("some value", 3);
  ("Test", 2), ("another value", 4);
]

let obj = {name="Button"; id=1; state=0}

let getwObjectName wObject =
  wObject.name

let getwObjectId wObject =
  wObject.id

let getwObjectState wObject =
  wObject.state

let checkwObjectState wObject checkState = 
  checkState = getwObjectState (wObject)

let incrementwObjectState wObject = 
  {wObject with state = + 1}


// Main program

// Test to check state
checkwObjectState obj 0

// Test to check state after update
let newObj = incrementwObjectState obj
printfn "%A" newObj
checkwObjectState newObj 0

let dictTest = [
  ("command1", "object1", 1),("result1", "newstate1");
  ("command2", "object2", 2),("result2", "newstate2");
  ("command3", "object3", 3),("result3", "newstate3");
]