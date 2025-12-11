namespace AOC

module Day7 =
    open Misc
    let input = [|
        ".......S.......";
        "...............";
        ".......^.......";
        "...............";
        "......^.^......";
        "...............";
        ".....^.^.^.....";
        "...............";
        "....^.^...^....";
        "...............";
        "...^.^...^.^...";
        "...............";
        "..^...^.....^..";
        "...............";
        ".^.^.^.^.^...^.";
        "...............";
    |]

    let private common input =
        let (start, len) = input |> Seq.head |> both (Seq.findIndex ((=) 'S')) String.length
        let inside = ((<=) 0) <&> ((>) len)
        let folder (splits, beams) (line:char array) =
            let splitBeam (c,p) = [(c,p-1);(c,p+1)] |> List.filter (snd >> inside) 
            let runsIntoSplitter = snd >> flip Array.item line >> ((=) '^')
            let combinePaths = both (snd >> List.sumBy fst) fst
            let split, unchanged = beams |> List.partition runsIntoSplitter 
            splits + List.length split, unchanged @ (split |> List.collect splitBeam) |> List.groupBy snd |> List.map combinePaths
        let traceBeams = Seq.tail >> Seq.map Seq.toArray >> Seq.fold folder (0, [(1L,start)])
        input |> traceBeams

    let private part1 = fst
    let private part2 = snd >> List.sumBy fst

    let solve: string seq -> string =
        common >> both part1 part2 >> output
    let test = input |> solve

