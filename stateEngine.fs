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
    |Read

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

// Input Command * string (or World Object name)
type InputTuple = Command * string

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
  // Entrance room puzzle interactions
  (Push, 1, 0), ("You push the button and hear a 'clunk' from the lever", [(1, 4, "The button is pushed in"); (2, 1, "The lever is upright")]);
  (Pull, 2, 0), ("You try to pull the lever, it will not move", []);
  (Pull, 2, 1), ("You pull the lever and as it slides down into place you hear a 'chink' sound from the door", [(2, 4, "The lever is pulled down"); (3, 1, "The door is unlocked")])
  (Open, 3, 0), ("You try the door, it does not move", []);
  (Open, 3, 1), ("You push door and it swings open", [(3, 4, "The door is open")]);
  // Tile room puzzle interactions
  (Read, 7, 4), ("You read the inscription, it says 'The moon raises the river at the end of the day'", [])
  (Move, 4, 0), ("You move the sun tile to the middle of the wall, but the moon and river tiles slide back", [(4, 1, "The sun tile is aligned in the middle of the wall"); (5, 2, "The moon tile is on the right side of the wall"); (6, 0, "The river tile is towards the bottom of the wall")])
  (Move, 5, 0), ("You move the moon tile to the middle of the wall, but the sun and river tiles slide back", [(5, 1, "The moon tile is aligned in the middle of the wall"); (4, 0, "The sun tile is on the left side of the wall"); (6, 0, "The river tile is towards the bottom of the wall")])
  (Move, 6, 0), ("You move the river tile to the middle of the wall, but the sun and moon tiles slide back", [(6, 1, "The river tile is aligned in the middle of the wall"); (4, 0, "The sun tile is on the left side of the wall"); (5, 0, "The moon tile is on the right side of the wall")])
  (Move, 4, 1), ("You move the sun tile back towards the left", [(4, 0, "The sun tile is on the left side of the wall"); (5, 0, "The moon tile is on the right side of the wall")])
  (Move, 5, 1), ("You move the moon tile back towards the right", [(5, 0, "The moon tile is on the right side of the wall"); (6, 0, "The river tile is towards the bottom of the wall")])
  (Move, 6, 1), ("You move the river tile back towards the bottom", [(6, 0, "The river tile is towards the bottom of the wall")])
  (Move, 4, 2), ("You move the sun tile back towards the left, but the moon tile also slides back", [(4, 0, "The sun tile is on the left side of the wall"); (5, 0, "The moon tile is on the right side of the wall")])
  (Move, 5, 2), ("You move the moon tile to the middle of the wall next to the sun tile", [(5, 4, "The moon tile is aligned with the sun tile in the middle of the wall"); (4, 2, "The sun tile is aligned with the moon tile in the middle of the wall"); (6, 3, "The river tile is towards the bottom of the wall")])
  (Move, 6, 3), ("You move the river tile to the middle of the wall in between the sun and moon tiles", [(6, 4, "The river tile is aligned with the sun and moon tiles in the middle of the wall"); (4, 4, "The sun tile is aligned with the moon and river tiles in the middle of the wall")])
]

// Entrance room objects
let obj1 = {id=1; name="button"; stateNum=0; stateString="The button is untouched"}
let obj2 = {id=2; name="lever"; stateNum=0; stateString="The lever is upright"}
let obj3 = {id=3; name="door"; stateNum=0; stateString="The door is closed"}


// Tile room objects
let sunTile = {id=4; name="sun tile"; stateNum=0; stateString="The sun tile is on the left side of the wall";}
let moonTile = {id=5; name="moon tile"; stateNum=0; stateString="The moon tile is on the right side of the wall";}
let riverTile = {id=6; name="river tile"; stateNum=0; stateString="The river tile is towards the bottom of the wall";}
let inscription = {id=7; name="inscription"; stateNum=4; stateString="The inscription reads 'The moon raises the river at the end of the day'";}

// Template - {id= ; name=""; stateNum=0; stateString="";}

let item1 = {id=0; name="dagger"}
let item2 = {id=1; name="sword"}


// =============================================================================================
// Test Rooms
let entranceRoom = {
  name = "Entrance";
  state = 0;
  items = Set.ofList [];
  objects = Set.ofList [obj1; obj2; obj3]
}

let tileRoom = {
  name = "Tile room";
  state = 0;
  items = Set.ofList [];
  objects = Set.ofList [sunTile; moonTile; riverTile]
}


// =============================================================================================

// Temp Variable - Create some sort of interface that alters the current location of the player
let mutable currentLocation = entranceRoom
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
      Seq.map (fun (object, (_, newStateNum, newStateString)) -> 
        {object with stateNum = newStateNum; stateString = newStateString}
      ) |> Set.ofSeq
  ]

(*
  Take in a list of updated objects and old object and adds the updated objects to a set, 
  removing the old ones, and generates a new Location record from this
*) 

let updateLocationObjectsSet (updateList:List<ObjectUpdateTuple>) = 
  let objectsList = updateObjectStates updateList
  {currentLocation with objects = Set.difference currentLocation.objects objectsList.Head |> Set.union(objectsList.Tail.Head)}

(*
Checks the interaction array to see if the command can be applied to the given object
- If it can, print out the resulting text and send back the list of object updates
- If nothing matches print error string, and return None
*) 
let checkInteractionKey (command:Command, wObject:WorldObject) =
  try 
    let (outputText,matchedItem) = interactionDict.Item((command, wObject.id, wObject.stateNum))
    printfn "%A" outputText
    Some matchedItem
  with _ ->  
  printfn "You cannot %A the %s" command wObject.name
  None

(*
  Processes a user input command based on the command keyword and the object it is being applied to
  Try and match the object name to an object with that name in the player's current location
  - Else return that there is no object of that name in the current location
  If found retrieve that object and check if there is an interaction between that object and the input command
  - If there is apply that command to the object, and check if the location state needs to be updated
  Update location state number if needed and return new location record
*)
let processCommand (command:Command, objectName:string) = 
  let wObject = currentLocation.objects |> Set.filter (fun object -> object.name = objectName) |> Set.toList |> List.tryHead
  match wObject with
  | Some wObject ->
    let checkResult = checkInteractionKey (command, wObject)
    printfn "%A" checkResult
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
// Testing function that runs through multiple inputs

let testPrintOutput location = 
  printfn "======================"
  printfn "Name     State"
  printfn "%s - %d"  location.name location.state
  location.objects |> Set.iter (fun object -> printfn "%s - %d" object.name object.stateNum)

let testHarness = 
  // First room test
  // testPrintOutput currentLocation
  // currentLocation <- processCommand (Pull, "lever")
  // testPrintOutput currentLocation
  // currentLocation <- processCommand (Push, "button")
  // testPrintOutput currentLocation
  // currentLocation <- processCommand (Open, "door")
  // testPrintOutput currentLocation
  // currentLocation <- processCommand (Pull, "lever")
  // testPrintOutput currentLocation
  // currentLocation <- processCommand (Open, "door")
  // testPrintOutput currentLocation
  // Second room test
  currentLocation <- tileRoom
  testPrintOutput currentLocation
  currentLocation <- processCommand (Read, "inscription")
  testPrintOutput currentLocation
  currentLocation <- processCommand (Move, "sun tile")
  testPrintOutput currentLocation
  currentLocation <- processCommand (Move, "moon tile")
  testPrintOutput currentLocation
  currentLocation <- processCommand (Move, "river tile")
  testPrintOutput currentLocation
  // currentLocation <- processCommand (Move, "sun tile")
  // testPrintOutput currentLocation
  // currentLocation <- processCommand (Move, "sun tile")
  // testPrintOutput currentLocation
  // currentLocation <- processCommand (Move, "sun tile")
  // testPrintOutput currentLocation

[<EntryPoint>]
let main argv = 
    //testHarness    
    processCommand (Push, "button")
    Console.ReadLine() |> ignore
    0

main

(*
UPDATED TO DO - 
Figure out how we want to update items

FOR THE OTHERS TO WORK ON - 
Need some user output methods that print out objects, items and information about rooms
Need some methods that interact with the user and take in input


GG EZ
We Gucci
*)