namespace AOC

module Day9 =
    open Misc

    let input = [|
        "7,1";
        "11,1";
        "11,7";
        "9,7";
        "9,5";
        "2,5";
        "2,3";
        "7,3";
    |]

    let parse = 
        let rects = dup >> uncurry Seq.allPairs >> Seq.toList
        let edges = 
            let final = both Seq.last Seq.head >> List.singleton
            let chain = Seq.pairwise >> Seq.toList
            (both final chain) >> uncurry (@)
        Seq.map (splitOn "," >> tuple >> tmap int) >> Seq.cache >> both edges rects

    let area ((x1,y1), (x2,y2)) = (abs(int64 x2 - int64 x1)+1L)*(abs(int64 y2 - int64 y1)+1L)

    let private part1 = snd >> Seq.maxBy area >> area

    let private part2 (edges, rects) =
        let intersects rect edge = (edge, rect) |> tmap BoundingBox.FromRect |> uncurry BoundingBox.intersectsInside
        let intersectsAnyEdge rect = edges |> List.tryFind(intersects rect) |> Option.isSome
        rects |> Seq.sortByDescending area |> Seq.find (intersectsAnyEdge >> not) |> area

    let solve: string seq -> string =
        parse >> both (timed part1) (timed part2) >> output
