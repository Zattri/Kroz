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
    |Spoon
    |Log
    |Nothing

        
let validateCommand(command:string, object:string) = 
    if command = "Use" then Use
    elif command = "Move" then Move
    elif command = "Help" then Help ["The sytax for this game is: command object. For example: Push Pillar. You can also use object ID's if you know them"]
    elif command = "Hint" then Hint
    elif command = "Rotate" then Rotate
    elif command = "Go" then Go
    elif command = "Search" then Search
    elif command = "Look" then Look
    elif command = "Open" then Open
    elif command = "Close" then Close
    elif command = "Push" then Push
    elif command = "Pull" then Pull
    else NotValid ["Please enter a valid command!"]

type result = 
    |Valid
    |Invalid of string list

let (<&>) inp1 inp2 x = //Perhaps this is wrong? Need to work on this some more as only examples are used.
  match inp1 x, inp2 x with
  | Use, Spoon -> Valid
  | Use, Log -> Invalid ["Can't use this command on this object!"]
  | Help[""], _ -> Valid
  |_, _ -> Invalid ["No command specified."]

let command = System.Console.ReadLine() //Takes in input for what command the user wants
let objoritm = System.Console.ReadLine() //Takes in input for item or object the user wants to use a command on

let validate = 
    validateCommand <&> //States this is incorrect

validate(command, objoritm) //Supposed to validate the inputs to make sure the user can perform a comman on and object or item