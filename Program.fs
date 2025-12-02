// For more information see https://aka.ms/fsharp-console-apps
let input = AOC.Input.load "2025" "1" |> Async.RunSynchronously

printfn "%A" (AOC.Day1.solve input)
