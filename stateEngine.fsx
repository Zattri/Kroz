// TYPE DEFINITIONS
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


// =====================================================================================
// DEFAULT VALUES FOR INTERACTIONS, ITEMS, OBJECTS AND LOCATIONS

// (Command, Object ID, Specific State Number) : (Interaction Text, New State Num, New State String)
let interactionDict = dict[
    (Push, 1, 0), ("You have pushed the button", 4, "The button on the wall is pushed in");
    (Pull, 2, 0), ("You have pulled the lever", 4, "The lever on the wall is pulled down");
]

let obj1 = {id=1; name="Button"; stateNum=0; stateString="The button is untouched"}
let obj2 = {id=2; name="Lever"; stateNum=0; stateString="The lever is upright"}

let item1 = {id=0; name="Dagger"}
let item2 = {id=1; name="Sword"}

let testLoc = {
    name = "Pyramid";
    state = 0;
    items = Set.ofList [item1; item2];
    objects = Set.ofList [obj1; obj2]
}

// =============================================================================================


// Error checking for function done if error tuple is returned or not from checkInteractionKey
let updatewObjectStates (wObject:WorldObject) (newStateNum:int) (newStateString:string) = 
  {wObject with stateNum = newStateNum; stateString = newStateString}

let updateLocationObjectsSet (wObject:WorldObject) (locationRecord:Location) (resultTuple:ResultTuple) = 
  let (_,newStateNum,newStateString) = resultTuple
  let newObject = updatewObjectStates wObject newStateNum newStateString
  {locationRecord with objects = locationRecord.objects.Remove(wObject).Add(newObject)}


// This function is really trash - Pls refine it, maybe 2 functions nested?
let formatInteractionAndErrorTuples (inputTuple:InputTuple) = 
  let (command, wObject) = inputTuple
  let errorString = String.Format("You cannot {0} on the {1}", command, wObject.name)
  ((command, wObject.id, wObject.stateNum), (errorString, wObject.stateNum, wObject.stateString))


// Checks the interaction array to see if the command can be applie to the given object
let checkInteractionKey (inputTuple:InputTuple) =
  let tuples = formatInteractionAndErrorTuples inputTuple
  // Need options on returning things
  [if (interactionDict.ContainsKey (fst tuples)) then
    yield interactionDict.Item(fst tuples) 
  else 
    yield (snd tuples)].Head // .Head is a bodge, any fixes?


// Also needs player location - testLoc needs to be where the object is located / where the player is currently (should be the same)
// - Make a function for that?
let processCommand (inputTuple:InputTuple) = 
let command,wObject = inputTuple
updateLocationObjectsSet wObject testLoc (checkInteractionKey (command, wObject))


// Testing Area - Careful, messy

printfn "%A" testLoc // Location before
processCommand (Push, obj1) // Location after

updateLocationObjectsSet obj1 testLoc (checkInteractionKey (Push, obj1))

checkInteractionKey (Pull, obj2)
checkInteractionKey (Push, obj2)

updatewObjectStates obj1 0 "The lever is upright"
updatewObjectStates obj1 1 "The lever is pulled"

(*
TODO - 
Need to add options to checkInteractionkey
Based on options update object or not
Figure out how we want to update items
- Update the sets in Locaton based off of this
  - Use the Set functions to update them and make new records
- Update Location state based off of whether all objects in the set are complete or not

GG EZ
We Gucci
*)