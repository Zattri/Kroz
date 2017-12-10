// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
#indent "off"

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


type Helpmsg = 
    { helpText: string }


type Hints = 
    { Room: Location;
      Counter: int;
      Message: string }

type Action = 
    { Name:string }

type command = //All commands in this type will be referenced to other types
    | Use of Action //When you perform an action command, the program should look for the action type.
    | Move of Action
    | Help of Helpmsg //Help will look fo the type Helpmsg
    | Hint of Hints //Hints will look for type Hints
    | Rotate of Action
    | Go of Action
    | Search of Location //Search will give details of objects in the room
    | Look of Location //Look will give details of everything in the room, including items and objects without specific detail
    | Open of Action
    | Close of Action
    | Push of Action
    | Pull of Action
    | NotValid of Helpmsg option //This will be looking for details with optional inputs. If nothing is typed, don't do anything and if something is typed but not in the list, return a help message


    let cmd = System.Console.ReadLine();
    if cmd == "" then
        command.Use
        else
            printfn "ERROR: Not a valid command or item! Type 'help' for info"