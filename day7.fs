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
            let splitBeam (c,p) = 
                match line[p] with
                | '.' -> [(c,p)]
                | '^' -> [(c,p-1);(c,p+1)] |> List.filter (snd >> inside) 
                | _ -> failwithf $"unexpected character at {p}: {line[p]}"
            let hitSplitter = snd >> flip Array.item line >> ((=) '^')
            let combinePaths = both (snd >> List.sumBy fst) fst
            let newSplits = beams |> List.filter hitSplitter |> List.length
            let newBeams = beams |> List.collect splitBeam |> List.groupBy snd |> List.map combinePaths
            splits + newSplits, newBeams
        let computation = Seq.tail >> Seq.map Seq.toArray >> Seq.fold folder (0, [(1L,start)])
        input |> computation

    let private part1 = fst
    let private part2 = snd >> List.sumBy fst

    let solve: string seq -> string =
        common >> both part1 part2 >> output

