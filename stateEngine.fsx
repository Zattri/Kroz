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

// objectID * newStateNum * newStateString
type ObjectUpdateTuple = int * int * string

// Interaction Output String * New stateNum * new stateString
type ResultTuple = string * List<ObjectUpdateTuple>


// =====================================================================================
// DEFAULT VALUES FOR INTERACTIONS, ITEMS, OBJECTS AND LOCATIONS

// (Command, Object ID, Specific State Number) : (Interaction Text, New State Num, New State String)
let interactionDict = dict[
    (Push, 1, 0), ("You have pushed the button", [(1, 4, "The button is pushed in"); (2, 4, "The lever is pulled")]);
    (Pull, 2, 0), ("You have pulled the lever", [(2, 4, "The lever is pulled"); (1, 4, "The button is pushed in")]);
]

// let newInteractionDict = dict[
//   (command, objId, stateOfObject), (resultTuple, [(ObjectUpdateTuple); (ObjectUpdateTuple)])
// ]

let obj1 = {id=1; name="button"; stateNum=0; stateString="The button is untouched"}
let obj2 = {id=2; name="lever"; stateNum=0; stateString="The lever is upright"}

let item1 = {id=0; name="dagger"}
let item2 = {id=1; name="sword"}

let testLoc = {
    name = "pyramid";
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

(*
  Function that updates the state of all objects objects in the room after an interaction
  - Filters the set of objects in the room down to only those that need to be altered
  - Combine the set of filtered objects with the list of new object stats
  - Create a set of WorldObjects with updated stats
  - Output a list with 2 sets, the oldObject that are to be removed from the room, and the new set of updated objects to be added
*)
let updateObjectStates (newObjStatsList:List<ObjectUpdateTuple>) =  
  let sortedObjStatsList = newObjStatsList |> List.sortBy (fun (id,_,_) -> id)
  let oldObjects = Set.filter (fun elem -> List.exists (fun (id,_,_) -> id = elem.id) sortedObjStatsList) currentLocation.objects
  [ oldObjects; 
    Seq.zip oldObjects sortedObjStatsList |> 
      Seq.map (fun item -> 
        let object, (_, newStateNum, newStateString) = item
        {object with stateNum = newStateNum; stateString = newStateString}
      ) |> Set.ofSeq
  ]


// Update the objects set at the current location, removing the old objects and adding the updated ones
let updateLocationObjectsSet (updateList:List<ObjectUpdateTuple>) = 
  let objectsList = updateObjectStates updateList
  {currentLocation with objects = currentLocation.objects |> Set.difference(objectsList.Head) |> Set.union(objectsList.Tail.Head)}

(*
Checks the interaction array to see if the command can be applied to the given object
- If it can, print out the resulting text and send back the list of object updates
- If nothing matches print error string, and return None
*) 
let checkInteractionKey (command:Command, wObject:WorldObject) =
  try 
    let outputText,matchedItem = interactionDict.Item(command, wObject.id, wObject.stateNum)
    printfn "%A" outputText
    Some matchedItem
  with _ ->  
  printfn "You cannot %A the %s" command wObject.name
  None

// Processes a user input command based on the command keyword and the object it is being applied to
let processCommand (command:Command, objectName:string) = 
  let wObject = currentLocation.objects |> Set.filter (fun object -> object.name = objectName) |> Set.toList |> List.tryHead
  match wObject with
  | Some wObject ->
    let checkResult = checkInteractionKey (command, wObject)
    match checkResult with
    | Some checkResult -> 
      let updatedLocation = updateLocationObjectsSet checkResult
      match updatedLocation.objects |> Set.forall (fun elem -> elem.stateNum = 4) with
      | true -> {updatedLocation with state = 1}
      | false -> updatedLocation
    | _ -> currentLocation
  | None -> 
    printfn "There is no %s in your current location" objectName
    currentLocation
    
  
  


// Testing Area - Careful, messy

currentLocation
processCommand (Pull, "lever")

(*
UPDATED TO DO - 
Convert to an actual program and not just a script
Figure out how we want to update items

FOR THE OTHERS TO WORK ON - 
Need some user output methods that print out objects, items and information about rooms
Need some methods that interact with the user and take in input


GG EZ
We Gucci
*)