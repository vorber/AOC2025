namespace AOC

module Day12 =
    open Misc

    (*let input = [|*)
    (*    "test"*)
    (*|]*)

    let part1 input =
        
        let shapeArea = 7
        let parse i =
            let parseLine line =
                let sz, vs = line |> splitOn ": " |> print "split %A" |> tuple
                let sx, sy = sz |> splitOn "x" |> Array.map int |> tuple
                let ts = vs |> splitOn " " |> Array.map int
                ((sx,sy), ts)
            i |> Seq.skip 30 |> print "%A" |> Seq.map parseLine

        let check (size, targets) =
            let area = size ||> (*) 
            let req = targets |> Seq.sumBy ((*) shapeArea)
            let fits = area >= req 
            printfn $"check area {area} for %A{targets} ({req}): {fits}"
            fits

        input
        |> parse
        |> Seq.filter check
        |> Seq.length

    let part2 input =
        input
        |> Const 0

    let solve: string seq -> string =
        both part1 part2 >> output

    let test = "test failed"
