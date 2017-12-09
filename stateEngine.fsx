open System

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
    stateNum:int;
    stateString:string;
  }

type Location = 
  {
    name:string;
    state:int;  
    items: Set<Item>;
    objects: Set<WorldObject>;
  }

// Input Command * WorldObject
type InputTuple = Command * WorldObject

// Input Command * Object ID * Specific Object State For Interaction
type InteractionTuple = Command * int * int

// Interaction Output String * New stateNum * new stateString
type ResultTuple = string * int * string


let checkwObjectState (wObject:WorldObject) checkState = 
  checkState = wObject.stateNum

let incrementwObjectState (wObject:WorldObject) = 
  {wObject with stateNum = wObject.stateNum + 1}



// Testing
let testLoc = {
    name = "Pyramid";
    state = 0;
    items = Set.ofList [
      {id=0; name="Dagger"};
      {id=1; name="Sword"}
    ];
    objects = Set.ofList [
      {id=1; name = "Pillar"; stateNum = 0; stateString="The pillar has an eagle carved into it"}; 
      {id=2; name="Door"; stateNum=1; stateString="The door has a glyph of a sun on it"}
    ]
}

// World Object definitions
let obj = {id=1; name="Button"; stateNum=0; stateString="The button is untouched"}
let obj2 = {id=2; name="Lever"; stateNum=0; stateString="The lever is upright"}


// Interaction Dictionary Definition 
// <Key> : <Item>
// (Command, Object ID, Specific State Number) : (Interaction Text, New State Num, New State String)
let interactionDict = dict[
    (Push, 1, 0), ("You have pushed the button", 4, "The button on the wall is pushed in");
    (Pull, 2, 0), ("You have pulled the lever", 4, "The lever on the wall is pulled down");
]


// This function is really trash - Pls refine it, maybe 2 functions nested?
let formatInteractionAndErrorTuples (inputTuple:InputTuple) = 
  let (command, wObject) = inputTuple
  let errorString = String.Format("You cannot {0} on the {1}", command, wObject.name)
  ((command, wObject.id, wObject.stateNum), (errorString, wObject.stateNum, wObject.stateString))


// Checks the interaction array to see if the command can be applie to the given object
let checkInteractionKey (inputTuple:InputTuple) =
  let tuples = formatInteractionAndErrorTuples inputTuple
  [if interactionDict.ContainsKey (fst tuples) then
    yield interactionDict.Item(fst tuples) 
  else 
    yield (snd tuples)]

checkInteractionKey (Push, obj)
checkInteractionKey (Pull, obj2)
checkInteractionKey (Push, obj2)
