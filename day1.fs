namespace AOC

module Day1 =
    open AOC.Misc
    let input = 
        [|
        "L68";
        "L30";
        "R48";
        "L5";
        "R60";
        "L55";
        "L1";
        "L99";
        "R14";
        "L82";
        |] :> seq<string>

    let parseItem (s:string)=
        match s[0] with
            | 'L' -> -int(s[1..])
            | 'R' -> int(s[1..])
            | _ -> failwithf "Invalid input %s" s

    let private part1 = 
        Seq.scan (fun x y -> (100 + x + y) % 100) 50
        >> Seq.filter ((=) 0)
        >> Seq.length

    let private part2 =
        let mc = multiple_count 100
        let cnt (a,b) =
            let counter = if a < b then mc else flip mc
            let count = counter a b
            if a % 100 = 0 then count - 1 else count
        Seq.scan (+) 50
        >> Seq.windowed 2
        >> Seq.map (tuple >> cnt)
        >> Seq.sum

    let solve input = 
        input
        |> Seq.map parseItem
        |> both part1 part2
