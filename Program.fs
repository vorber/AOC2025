// For more information see https://aka.ms/fsharp-console-apps
open System.CommandLine
open AOC

let solveinput day = 
    match day with
    | 1 -> Day1.solve
    | 2 -> Day2.solve
    | _ -> failwithf "Day %i is not there yet" day

let solve day =
    let input = AOC.Input.load "2025" (string day) |> Async.RunSynchronously
    printfn "%A" (solveinput day input)
    0

let test day =
    let res = 
        match day with
        | 1 -> solveinput day Day1.input
        | 2 -> solveinput day Day2.input
        | _ -> failwithf "Day %i is not there yet" day
    printfn "%A" res

let dayOption = Option<int>("--day", Description="Select which day to run")
let solveCommand = 
    let cmd = Command("solve", "Run solver on full input")
    cmd.SetAction(fun pr -> dayOption |> pr.GetValue |> solve)
    cmd

let testCommand = 
    let cmd = Command("test", "Run solver on test input")
    cmd.SetAction(fun pr -> dayOption |> pr.GetValue |> test)
    cmd

let rootCommand =
    RootCommand("Advent of Code 2025")
    |> fun cmd ->
        cmd.Add(dayOption)
        cmd.Add(solveCommand)
        cmd.Add(testCommand)
        cmd

[<EntryPoint>]
let main argv = 
    rootCommand.Parse(argv).Invoke()


