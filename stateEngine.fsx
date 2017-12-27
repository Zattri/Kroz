// TYPE DEFINITIONS
open System
open System.Security.AccessControl.NativeObjectSecurity
open System.Xml.Xsl
open System.Security.AccessControl

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

// objectID * newStateNum * newStateString
type ObjectUpdateTuple = int * int * string

// Interaction Output String * New stateNum * new stateString
type ResultTuple = string * List<ObjectUpdateTuple>


// =====================================================================================
// DEFAULT VALUES FOR INTERACTIONS, ITEMS, OBJECTS AND LOCATIONS

// (Command, Object ID, Specific State Number) : (Interaction Text, New State Num, New State String)
let interactionDict = dict[
    (Push, 1, 0), ("You have pushed the button", 4, "The button on the wall is pushed in");
    (Pull, 2, 0), ("You have pulled the lever", 4, "The lever on the wall is pulled down");
]

let newInteractionDict = dict[
  (command, objId, stateOfObject), (resultTuple, [(ObjectUpdateTuple); (ObjectUpdateTuple)])
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
// Sands of Time test room 
let sandsOfTime = {
  name = "Sand Room";
  state = 0;
  items = Set.ofList [];
  objects = Set.ofList [obj1; obj2]
}


// =============================================================================================

// Temp Variable - Create some sort of interface that alters the current location of the player
let currentLocation = sandsOfTime
// =============================================================================================

let objUpdateList = [(1, 3, "The lever is pulled"); (2, 3, "The button is pushed")]

let rec createObjIdList objList =
  match objList with
  | [(id,_,_)] -> [id]
  | (id,_,_)::xs -> id::createObjIdList xs

(* THE PLAN - 
    Want to return a new set of objects for the current location

    For each item in the list return the corresponding item from the set of objects, based on the object id
    Create a new object record with updated stateNum and stateString - ALREADY HAVE A FUNCTION
    Remove the old object from the set and add the new object to the set - ALREADY IN A FUNCTION, TAKE IT OUT AND ADD TO HERE
    Return the final set after iterating through the list of updated objects ends
  *) 

  
  // Use all these elements to generate new elements from them
  // Remove all the old elements from the set and add the new ones to the set
let newUpdateObjectState (newObjStatsList:List<ObjectUpdateTuple>) = 
  let objectsToUpdate = Set.filter (fun elem -> List.exists (fun (id,_,_) -> id = elem.id) newObjStatsList) currentLocation.objects
  let newObjects = Set.empty
  // This is broken
  List.map (fun (id, newStateNum, newStateString) ->
    Set.map (fun oldObject -> 
      if (oldObject.id = id) then
        newObjects.Add({oldObject with stateNum = newStateNum; stateString = newStateString})
    ) objectsToUpdate
  ) newObjStatsList
  newObjects
  // Iterate through the ObjStatsList and match ids to records in the set
  // Create a new set with new records that contain the updated stats based on the old records
  // Delete the set of old records from the location objects set
  // Add the set of new records to the location objects set
  

newUpdateObjectState objUpdateList

// Error checking for function done if error tuple is returned or not from checkInteractionKey
let updatewObjectStates (wObject:WorldObject) (newStateNum:int) (newStateString:string) = 
  {wObject with stateNum = newStateNum; stateString = newStateString}

// Alter this to always use the current location
let updateLocationObjectsSet (wObject:WorldObject) (locationRecord:Location) (outputText:string, newStateNum:int, newStateString:string) = 
  let newObject = updatewObjectStates wObject newStateNum newStateString
  {locationRecord with objects = locationRecord.objects.Remove(wObject).Add(newObject)}


// This function is really trash - Pls refine it, maybe 2 functions nested?
let formatInteractionAndErrorTuples (command:Command, wObject:WorldObject) =
  let errorString = String.Format("You cannot {0} on the {1}", command, wObject.name)
  ((command, wObject.id, wObject.stateNum), (errorString, wObject.stateNum, wObject.stateString))


// Checks the interaction array to see if the command can be applie to the given object
let checkInteractionKey (inputTuple:InputTuple) =
  let keyTuple,errorTuple = formatInteractionAndErrorTuples inputTuple
  // Need options on returning things
  [if (interactionDict.ContainsKey keyTuple) then
    yield interactionDict.Item(keyTuple) 
  else 
    yield errorTuple].Head
// Try using filter for if then else lines


let processCommand (command:Command, wObject:WorldObject) = 
  updateLocationObjectsSet wObject currentLocation (checkInteractionKey (command, wObject))


// Testing Area - Careful, messy

testLoc // Location before
processCommand (Push, obj1) // Location after

updateLocationObjectsSet obj1 currentLocation (checkInteractionKey (Push, obj1))

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
Need to add a method of checking if every object in a location is complete, and update the location state

FOR THE OTHERS TO WORK ON - 
Need some user output methods that print out objects, items and information about rooms
Need some methods that interact with the user and take in input


GG EZ
We Gucci
*)