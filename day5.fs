namespace AOC

module Day5 =
    open Misc
    open System

    let input = [|
        "3-5";
        "10-14";
        "16-20";
        "12-18";
        "";
        "1";
        "5";
        "8";
        "11";
        "17";
        "32";
    |]

    let parse =
        let split = Seq.toList >> splitWhen (String.IsNullOrWhiteSpace) >> List.toArray >> tuple
        let parseRange = List.map (splitOn "-" >> tuple >> tmap int64) >> List.sortBy fst
        let rec mergeRanges acc ranges =
            match ranges with
            | [] -> acc
            | (l1,h1)::(l2,h2)::t when l2 <= h1 + 1L -> mergeRanges acc ([(min l1 l2,max h1 h2)] @ t)
            | h::t -> mergeRanges (acc@[h]) t
        let parseValue = List.map int64
        split >> tmap2 (parseRange >> mergeRanges [] >> List.toArray) parseValue


    type Comparison = Below | Inside | Above
    let rec containedIn ranges (value:int64) =
        let compare (l,h) x = if x < l then Below else if x > h then Above else Inside
        match Array.length ranges with
        | 0 -> None
        | x -> let m = x/2
               match (compare ranges[m] value) with
               | Below -> containedIn ranges[..m-1] value
               | Above -> containedIn ranges[m+1..] value
               | Inside -> Some value

    let private part1 (ranges, values) =
        values
        |> List.choose (containedIn ranges)
        |> List.length

    let private part2 = fst >> Array.map (fun (l,h)->h-l+1L) >> Array.sum

    let solve: string seq -> string =
        parse >> both part1 part2 >> output
    let test = input |> solve
