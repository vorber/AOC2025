open System.CommandLine
open AOC

let data day = 
    match day with
    | 1 -> (Day1.solve, Day1.test)
    | 2 -> (Day2.solve, Day2.test)
    | 3 -> (Day3.solve, Day3.test)
    | 4 -> (Day4.solve, Day4.test)
    | 5 -> (Day5.solve, Day5.test)
    | 6 -> (Day6.solve, Day6.test)
    | 7 -> (Day7.solve, Day7.test)
    | 8 -> (Day8.solve, Day8.test)
    | 9 -> (Day9.solve, Day9.test)
    | 10 -> (Day10.solve, Day10.test)
    | 11 -> (Day11.solve, Day11.test)
    | 12 -> (Day12.solve, Day12.test)
    | _ -> failwithf "Day %i is not there yet" day

let solveinput = data >> fst

let solve day =
    let input = AOC.Input.load "2025" (string day) |> Async.RunSynchronously
    printfn "%A" (solveinput day input)

let test = data >> snd >> printfn "%A"

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


