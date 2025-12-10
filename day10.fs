namespace AOC

module Day10 =
    open Misc
    open FParsec
    open Flips
    open Flips.Types

    let input = [|
        "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}";
        "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}";
        "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}";
    |]

    let private parseLine line =
        let str = pstring
        let intList o c = str o >>. sepBy pint32 (str ",") .>> str c
        let mask = 
            let maskfolder acc c =
                match c with
                | "." -> acc <<< 1
                | "#" -> (acc <<< 1) + 1
                | _ -> acc
            str "[" >>. many (str "." <|> str "#") .>> str "]" 
                |>> (List.rev >> List.fold maskfolder 0)
        let buttons = many (intList "(" ")" .>> str " ")
        let full = tuple3 (mask .>> spaces) (buttons .>> spaces) (intList "{" "}")
        match (run full line) with
        | Success(result, _, _) -> result
        | Failure(msg, _, _) -> failwith msg

    let parse = Seq.map parseLine

    let private part1 =
        let reach buttons target =
            let flipBits v bits =
                let mask = bits |> List.sumBy ((<<<) 1)
                v ^^^ mask
            let rec bfs steps visited queue =
                if queue |> Set.contains target then steps
                else 
                    let next v = buttons |> List.map (flipBits v) |> Set.ofList
                    let queue' = (queue |> Seq.collect next |> Set.ofSeq) - visited
                    let visited' = visited + queue
                    bfs (steps+1) visited' queue' 
            bfs 0 (Set.empty) (Set.singleton 0) 
        let count (target, buttons, _) = reach buttons target
        Seq.sumBy count

    let private part2 =
        let count (_, buttons, target) =
            let buttons = buttons |> List.map (Set.ofList)
            let bc = List.length buttons
            let ts = List.length target
            let numberOfPushes = Seq.init bc (fun i -> i, Decision.createInteger $"Button {i} pushes" 0.0 infinity) |> Map.ofSeq
            let np n = 1.0*numberOfPushes[n]
            let objective =
                let expr = numberOfPushes |> Map.values |> Seq.sumBy ((*) 1.0)
                Objective.create "Minimize button pushes" Minimize expr
            let positivityConstraints = Seq.init bc (fun i -> Constraint.create $"positivity {i}" (numberOfPushes[i] >== 0.0))
            let pressButtons ns =
                let pressButton (b, times) = List.init ts (fun i -> if Set.contains i b then times else LinearExpression.Zero)
                let combine l1 l2 =
                    (l1, l2) ||> List.zip |> List.map (uncurry (+))
                (buttons, ns) ||> List.zip |> List.map pressButton |> List.reduce combine
            let targetContraints =
                let pushes = List.init bc np
                pressButtons pushes
                |> List.zip (List.map float target) 
                |> List.map (uncurry (==))
                |> List.mapi (fun i c -> Constraint.create $"target joltage {i}" c) 
            let model =
                Model.create objective
                |> Model.addConstraints positivityConstraints
                |> Model.addConstraints targetContraints
            let settings = {
                SolverType = SolverType.CBC
                MaxDuration = 10_000L
                WriteLPFile = None
                WriteMPSFile = None
            }
            let result = Solver.solve settings model
            match result with
            | Optimal solution -> (Objective.evaluate solution objective)
            | _ -> failwithf $"Unable to solve. Error: %A{result}"
        Seq.sumBy count >> int

    let solve: string seq -> string =
        parse >> both part1 part2 >> output

