//Todo: Get user input and match it against the list of commands.
//Todo: Match commands with items and objects, restrict what can happen.
//Todo: Pass this over to the game engine.

open System
open System.Text.RegularExpressions
//Some of these types are EXAMPLES and are to be replaced later on. These are clearly marked.

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


type Hints =  //Hints are based on location and an integer, if the integer is 0, no hints are given.
    { Room: Location;
      Counter: int;
      Message: string }

type Command = //All Commands are here
    | Use
    | Move
    | Help
    | Hint
    | Rotate
    | Go
    | Search
    | Look
    | Open
    | Close
    | Push
    | Pull
    | NotValid

type object = //EXAMPLE
    | Chest
    | Piller
    | Wall
    | Button
    | Lever
    | Plinth


        
let execCommand command = //Do something based on the type of command given
    match command with
    | Use -> printfn "Use Command"
    | Move -> printfn "Move Command"
    | Help -> printfn "Help Command"
    | Hint -> printfn "Hint Command"
    | Rotate -> printfn "Rotate Command"
    | Go -> printfn "Go Command"
    | Search -> printfn "Search Command"
    | Look -> printfn "Look Command"
    | Open -> printfn "Open Command"
    | Close -> printfn "Close Command"
    | Push -> printfn "Push Command"
    | Pull -> printfn "Pull Command"
    | NotValid -> printfn "Not Valid"
    
(*type result = 
    |Valid
    |Invalid of string list*)
    
let useCapture = "^\s*use\s*\w*\s*|\s*u\s*\w*\s*$" //Regular expression to catch possible mistakes
let helpCapture = "^\s*help\s*|\s*h+\s*$"
let moveCapture = "^\s*move\s*\w*\s*|\s*m\s*\w*\s*$"
let hintCapture = "^\s*hint\s*|\s*h\s*$"
let rotateCapture = "^\s*rotate\s*\w*\s*|\s*r\s*\w*\s*$"
let GoCapture = "^\s*go\s*\w*\s*|\s*g\s*\w*\s*$"
let searchCapture = "^\s*search\s*\w*\s*|\s*s\s*\w*\s*$"
let lookCapture = "^\s*look\s*\w*\s*|\s*l\s*\w*\s*$"
let openCapture = "^\s*open\s*\w*\s*|\s*o\s*\w*\s*$"
let closeCapture = "^\s*close\s*\w*\s*|\s*c\s*\w*\s*$"
let pushCapture = "^\s*push\s*\w*\s*|\s*pus\s*\w*\s*$"
let pullCapture = "^\s*pull\s*\w*\s*|\s*pul\s*\w*\s*$"

let (|UseMatch|MoveMatch|HelpMatch|HintMatch|RotateMatch|GoMatch|NoMatch|) input = //All the match cases are in the brackets
    match Regex.Match(input, useCapture), Regex.Match(input, moveCapture), Regex.Match(input, helpCapture),
    Regex.Match(input, hintCapture), Regex.Match(input, rotateCapture), Regex.Match(input, GoCapture)
     with //these are like rules, in order, each comma seporates these.
        |useMatch, _, _, _, _, _ when useMatch.Success -> //Look at each rule and see if this matches the input given.
            UseMatch //Determine a match was found and what match it was
        |_, moveMatch, _, _, _, _ when moveMatch.Success ->
            MoveMatch
        |_, _, helpMatch, _, _, _ when helpMatch.Success ->
            HelpMatch
        |_, _, _, hintMatch, _, _ when hintMatch.Success ->
            HintMatch
        |_, _, _, _, rotateMatch, _ when rotateMatch.Success ->
            RotateMatch
        |_, _, _, _, _, goMatch when goMatch.Success ->
            GoMatch
        |_ ->
            NoMatch

let (|SearchMatch|LookMatch|OpenMatch|CloseMatch|PushMatch|PullMatch|NoMatch|) input = //Second Active Pattern Match
        match Regex.Match(input, searchCapture), Regex.Match(input, lookCapture), Regex.Match(input, openCapture),
        Regex.Match(input, closeCapture), Regex.Match(input, pushCapture), Regex.Match(input, pullCapture) with
        |searchMatch, _, _, _, _, _ when searchMatch.Success ->
            SearchMatch
        |_, lookMatch, _, _, _, _ when lookMatch.Success ->
            LookMatch
        |_, _, openMatch, _, _, _ when openMatch.Success ->
            OpenMatch
        |_, _, _, closeMatch, _, _ when closeMatch.Success ->
            CloseMatch
        |_, _, _, _, pushMatch, _ when pushMatch.Success ->
            PushMatch
        |_, _, _, _, _, pullMatch when pullMatch.Success ->
            PullMatch
        |_ ->
            NoMatch //Determine no match was found, matching this with NoMatch

let defineInput input = //Based on Active Pattern matches, link it to the list of commands.
    match input with
    |UseMatch -> Use
    |MoveMatch -> Move
    |HelpMatch -> Help
    |HintMatch -> Hint
    |RotateMatch -> Rotate
    |GoMatch -> Go
    |SearchMatch -> Search
    |LookMatch -> Look
    |OpenMatch -> Open
    |CloseMatch -> Close
    |PushMatch -> Push
    |PullMatch -> Pull
    |_ -> NotValid

let parseExec input = //Define the input from the user input (match it) and then execute the command.
    defineInput input
    |> execCommand



   (* 
 Descrpitons :
 Entry room - A large open room lays before you with a path to your east, west and north
 behind you lies a great stone door from where you entered blocking any attempt at escaping




 Storage room - There are racks filled with broken and old tools. There are several skeletons on the floor
 all look as if they died long ago. Their bones are bleach white andcovered on cobwebs.
Hint - That skeleton seems to be clutching something
Look at skeletons - One skeleton stands out at you more than all the others, he appears to be clutching a metal hammer of some kind


 Puzzle Room - The room has 5 holes in the eastern wall and a wall with some sort of textscralled into it along with a stone door blocking you from going any further into the dungeon
Hint - Maybe the text has a way to get out of here
 Look at holes- behind each hole is a lever.
 Look at text - Scralled into the sandstone wall reads "2 plus 2 is " and then reads "-1 thats  quik mafs "

 Damp room - The passages to the south and east are blocked with two long channels running from the west to the east wall
 One of the channels has water flowing down it but the other does not but the wall behind the channel is damp and cracked.

 Piller room - Three pillers stand before you and a crack from the ceiling shines sunlight down from above on to a metal framework stand. to your north is another locked metal door
 Hint - Maybe something fits in the metal stand
 Hint (if crystal placed) - The light beam stops at the last piller but passes through a hole in the other two. I have to alter the last piller somehow

 Challenge room - before you lies a plinth with a small sack on top alone in a large room
 Hint - There are scratch marks on the floor by the plinth traveling eastwards, maybe the plinth moves
 Look at plinth - The plinth is made of stone and is stange to be the only thing standing in the room. However the plinth has marks on the floor by it heading to the eastern wall
 Look at sack - The sack lies on top of the plinth filled to the brim with gems and gold coins and is not attached to the plinth in anyway

 Key Room - Inside this tiny room is only one small thing. A large key on a wooden table.
 Look at key - A large metal key with a crown engraved onto its side.

 Treasure room - A large chest lies in the center of the room
 Look at chest - A golden trimmed wooden chest which haskept in suprisingly imaculate condition over the years. The chest is ajar and you can see the gleem of light reflect from your torch off the golden inside.

 Common room - Filled with old wooden tables and benches. The floor is coated in a fine bleach white powder (COCAINE) that isnt dust but you cnat make it out
 Around the tables there are one or two skeletons but they are missing bones.
 Look at tables - Old wooden tables that are of no use but a item lays on top coating in this white dust
 Look at skeletons - The skeletons are missing their legs they are no where to be found in this room.
 Hint - The item is a crystal of somesort maybe it will come in use later in here.
 *)
let Location = dict["Entry","Entry Room"; "Storage", "Storage Room"; "Puzzle", "Puzzle Room"; "Bath", "Bathhouse"; "Piller", "Piller Room"; "Challenge", "Dark Room"; "Key", "Key Room"; "Treasure", "Treasure Room"; "Common", "Common Room";]
(*
 let WObjects = ("Stand" , "Piller" ,"Wall", "Hole 1" , "Hole 2" ,"Hole 3" ,"Hole 4" ,"Hole 5" , "lever 1" , "lever 2" , "lever 3" , "lever 4" , "lever 5" , "plinth" , "Treasure chest")

 let Items = [| "Hammer" ; "Crystal";  "Sack of Gold"; "Stone Key"; "Golden Key"; |]
 let ItemsComm = [|"Crystal"|]
 let ItemsStor = [|"Hammer"|]
 let ItemsDark = [|"Sack of Gold"; "Stone Key"|]
 let ItemsKey = [|"Golden Key"|]
 let ItemsTreas = [|"Treasure"|]
 type Map =
  let EntryCon = dict["South", "Common Room"; "East", "Puzzle Room"; "West", "Storage Room";]
  let CommonCon = dict["South", "Treasue Room"; "West", "Storage Room";]
  let StoreCon = dict["South", "Common Room"; "East", "Entry Room"; ]
  let PuzzleCon = dict["South", "Bathhouse"; "West", "Entry Room";]
  let BathhouseCon = dict["South", "Key Room"; "East", "Piller Room"; "North", "Puzzle Room";]
  let PillerCon = dict["North", "Dark Room"; "West", "Bathhouse";]
  let DarkCon = dict["South", "Piller Room";]
  let KeyCon = dict["North", "Bathhouse";]
  let TreasureCon = dict["North", "Common Room";] *)


  


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
                        (Rotate, 4 , 0), ("The piller rotates smoothly around on its axis",[(4, 4, "The piller has been rotated into place")]);
                        (Rotate, 4, 4), ( "The piller cannot be rotated any further",[]);
                        (Pull, 2, 0), ("The lever will not move", []);
                        (Pull, 2, 1), ("The lever clunks down into place and there is a 'chink' sound from the door", [(2, 4, "The lever is pulled down"); (3, 1, "The door is unlocked")])
                        (Open, 3, 0), ("The door does not move", []);
                        (Open, 3, 1), ("The door swings open", [(3, 4, "The door is open")]);
                        (Open, 1, 1), ("The chest swings open to reaveal a heap of treasure comprised of gold and gems", [(1, 4, "The chest is wide open")]); 
                        (Push, 6, 0), ("The plinth is pushed asside revealing a small stone key",[(6,4, "The plinth is pushed aside against the wall")])
]

let obj1 = {id =1; name="chest"; stateNum=1; stateString="The chest lies flat against the back wall ajar"}
let obj2 = {id=2; name="lever"; stateNum=0; stateString="The lever is upright"}
let obj3 = {id=3; name="door"; stateNum=0; stateString="The door is closed"}
let obj4 = {id=4; name="piller"; stateNum=0; stateString="The piller stands tall in the room"}
let obj5 = {id=5; name= "wall"; stateNum=0; stateString="The wall is damp and cracked with trickles of water seeping from it"}
let obj6 = {id =6; name= "plinth"; stateNum=0; stateString= "The plinth stands in the centre of the room"}
let obj7 = {id=7; name= "stand"; stateNum=0; stateString="A metal framework stand which has rusted from a thousand years of idleness"}

let item1 = {id=0; name="hammer"}
let item2 = {id=1; name="crystal"}
let item3 = {id=2; name="stone Key"}
let item4 = {id=3; name="sack of gold"}
let item5 = {id=4; name="golden key"}
let item6 = {id=5; name="treasure"}


// =============================================================================================
// all rooms 
let entry = {
  name = "Entry Room";
  state = 0;
  items = Set.ofList [];
  objects = Set.ofList [obj3];
}
let storage = {
    name = "Storage Room";
    state = 1;
    items = Set.ofList [item1;]
    objects = Set.ofList [];
}
let puzzle = {
    name="Puzzle Room";
    state = 2;
    items = Set.ofList[];
    objects = Set.ofList [obj2; obj3;] //one door
}
let bath = {
    name = "Bathhouse";
    state = 4;
    items= Set.ofList [];
    objects= Set.ofList[obj3; obj5]; //3doors
}
let common = {
    name = "Common Room";
    state = 3;
    items= Set.ofList [item2;]
    objects= Set.ofList [obj3;] //1 door
}
let piller = {
    name = "Piller Room";
    state = 5;
    items = Set.ofList [];
    objects = Set.ofList [obj3; obj4];
}
let challenge = {
    name = "Dark Room";
    state = 6;
    items= Set.ofList [item4; item3];
    objects= Set.ofList[obj3; obj6];
}
let key = {
    name = "Key Room";
    state = 7;
    items= Set.ofList [item5;]
    objects= Set.ofList [obj3]
}
let treasure = {
    name = "Treasure Room";
    state = 8;
    items = Set.ofList [item6];
    objects= Set.ofList [obj3; obj1];
}
let nothing = {
    name= "ReeceIsABarnOwl";
    state= 10;
    items = Set.ofList [];
    objects= Set.ofList [];
}
let endgame = {
    name= "Its All Ogre";
    state= 9;
    items= Set.ofList [];
    objects= Set.ofList[];
}
(*let map = [(entry, endgame, puzzle, nothing, storage); (storage, nothing, entry, common, nothing);
            (common, storage, nothing, treasure, nothing); (treasure, common, nothing, nothing, nothing);
            (bath, puzzle, piller, key, nothing); (piller, challenge, nothing, nothing, bath);
            (challenge, nothing, nothing, piller, nothing);(key, bath, nothing, nothing, nothing);]
                |> Map.ofList;;

            val map : Map<Location, Location, Location, Location, Location>*)
type Tree =
            | Branch of Location * Tree list
            | Leaf of Location
let treeMap = Branch(entry, [Branch (storage, [ Branch (common, [Leaf treasure])]); Branch (puzzle, [ Branch (bath,  [Branch (piller,  [Leaf challenge; Leaf key];)])])])
let rec checkstuff treeMap =
    match treeMap with
    | Leaf _ -> true
    | Branch (node, children) ->
        List.fold ( || ) false (List.map checkstuff children)
// =============================================================================================

// Temp Variable - Create some sort of interface that alters the current location of the player
type inventoryState = {location:Location; Inventory:String List[]}
let currentLocation = entry
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
// Testing function that runs through multiple inputs
(*let testHarness =
  printfn "%A" currentLocation
  currentLocation <- processCommand (Pull, "lever")
  printfn "%A" currentLocation
  currentLocation <- processCommand (Push, "button")
  printfn "%A" currentLocation
  currentLocation <- processCommand (Open, "door")
  printfn "%A" currentLocation
  currentLocation <- processCommand (Pull, "lever")
  printfn "%A" currentLocation
  currentLocation <- processCommand (Open, "door")
  printfn "%A" currentLocation*)
  
//Collect all states into one and represent it here, states from other areas should be updated via this overall state, pass in the string and make it a command
Seq.fold

[<EntryPoint>]
let main argv =
    //testHarness
    let inputSeq = 
            seq { 
                while true do 
                    printf "Command: "
                    yield System.Console.ReadLine().ToLower()
            }
    // Seq.map (fun x -> x) inputSeq
    let x = Seq.take 10 inputSeq
    printfn "%A" x
    //Console.ReadLine() |> ignore
    0
(*[<EntryPoint>] //For testing purposes
let main args =
    let input = seq {
        yield! inputSeq
    }
    input 
    |> ignore
    0 *)
(*
UPDATED TO DO -
Figure out how we want to update items
FOR THE OTHERS TO WORK ON -
Need some user output methods that print out objects, items and information about rooms
Need some methods that interact with the user and take in input
GG EZ
We Gucci
*)
