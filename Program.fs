open System.Diagnostics
open System.Runtime.Remoting.Messaging

//Some of these types are EXAMPLES and are to be replaced later on. These are clearly marked.

type Item =  //EXAMPLE
  {
    id:int;
    name:string; 
  }
  

type WorldObject =  //Not my code
    {
     id:int;
     name:string;
     stateNum:int;
     stateString:string;
    }

type Location = //Not my code
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

type command = //All Commands are here
    | Use
    | Move
    | Help of string list
    | Hint
    | Rotate
    | Go
    | Search
    | Look
    | Open
    | Close
    | Push
    | Pull
    | NotValid of string list

type object = //EXAMPLE
    | Chest
    | Piller
    | Wall
    | Button
    | Lever
    | Plinth

        
//let validateCommand(command:string, object:string) = 
    //TODO

type result = 
    |Valid
    |Invalid of string list

let (<&>) inp1 inp2 x = //Perhaps this is wrong? Need to work on this some more as only examples are used.
  match inp1 x, inp2 x with
  | Use, Plinth -> Valid
  | Use, _ -> Invalid ["Can't use this command on this object!"]
  | Move, Piller -> Valid
  | Move, Plinth -> Valid
  | Move, _ -> Invalid ["You can't move it object, try 'Move Pillar' or 'Move Plinth'"]
  | Rotate, Piller -> Valid
  | Rotate, Plinth -> Valid
  | Rotate, _ -> Invalid ["This type of object cannot be rotated! Try using this command with another object."]
  | Search, Chest -> Valid
  | Search, Plinth -> Valid
  | Search, _ -> Invalid ["You cannot search this object. Try searching another object."]
  | Help[""], _ -> Valid
  |_, _ -> Invalid ["No command specified."]

let command = System.Console.ReadLine().Trim().ToLower() //Takes in input for what command the user wants
let objoritm = System.Console.ReadLine().Trim().ToLower() //Takes in input for item or object the user wants to use a command on

let validate = 
    validateCommand <&> //States this is incorrect

validate(command, objoritm) //Supposed to validate the inputs to make sure the user can perform a comman on and object or item