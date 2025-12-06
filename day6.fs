namespace AOC

module Day6 =
    open Misc
    let input = [|
        "123 328  51 64 ";
        " 45 64  387 23 ";
        "  6 98  215 314";
        "*   +   *   +  ";
    |]

    let split = splitOn " " >> Seq.filter ((<>) "")
    let parseOp c =
        match c with
        | "*" -> (*)
        | "+" -> (+)
        | _ -> failwithf $"unknown op {c}"

    let parse_part1 = Seq.map split >> Seq.transpose >> Seq.map Seq.rev
    let part1 =
        let calcLine = both (Seq.head >> parseOp) (Seq.tail >> Seq.map int64) >> uncurry Seq.reduce 
        Seq.map calcLine >> Seq.sum

    let parse_part2 =
        let ops = Seq.head >> split >> Seq.map parseOp >> Seq.rev
        let nums =
            Seq.tail >> Seq.transpose >> Seq.rev
            >> Seq.map (Seq.rev >> Seq.toArray >> System.String)
            >> Seq.toList >> splitWhen (System.String.IsNullOrWhiteSpace)
            >> List.map (List.map int64)
        Seq.rev >> (both ops nums)

    let part2 = uncurry Seq.zip >> Seq.map (uncurry List.reduce) >> Seq.sum

    let solve: string seq -> string =
        both (parse_part1 >> part1) (parse_part2 >> part2) >> output
