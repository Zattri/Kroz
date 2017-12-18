//Todo: Get user input and match it against the list of commands.
//Todo: Match commands with items and objects, restrict what can happen.
//Todo: Pass this over to the game engine.

open System
open System.Text.RegularExpressions
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

let inputSeq = seq { //Get input from the user
    while true do
        printf "Your Command: "
        yield System.Console.ReadLine().Trim().ToLower()//Take user input and convert all characters to lower case
    }

let parseExec input = //Define the input from the user input (match it) and then execute the command.
    defineInput input
    |> execCommand

[<EntryPoint>] //For testing purposes
let main args =
    let input = seq {
        yield! inputSeq
    }
    input 
    |> ignore
    0