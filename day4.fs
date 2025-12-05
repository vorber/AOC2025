namespace AOC

module Day4 = 
    open Misc
    let input = [|
        "..@@.@@@@.";
        "@@@.@.@.@@";
        "@@@@@.@.@@";
        "@.@@@@..@.";
        "@@.@@@@.@@";
        ".@@@@@@@.@";
        ".@.@.@.@@@";
        "@.@@@.@@@@";
        ".@@@@@@@@.";
        "@.@.@@@.@.";
    |]

    let neighbors = 
        seq {
            (-1,-1); (0,-1); (1,-1);
            (-1, 0);         (1, 0);
            (-1, 1); (0, 1); (1, 1);
        }

    let move (x,y) (dx,dy) = x+dx,y+dy

    let parseInput input =
        let parseLine line = Seq.mapi (fun i a -> ((i, line), a))
        let rollPositions = Seq.mapi parseLine >> Seq.concat >> Seq.filter (snd >> (=) '@') >> Seq.map fst >> Set
        let positions = rollPositions input
        let countNeighbors p = 
            neighbors 
            |> Seq.map (move p) 
            |> Seq.filter (flip Set.contains positions)
            |> Seq.length
        positions |> Set.map (both countNeighbors id)

    let isNeighbor (x1,y1) (x2,y2) =
        (abs(x1-x2) = 1 && abs(y1-y2) <= 1) || (abs(y1-y2) = 1 && abs(x1-x2) <= 1)

    let findNeighbors ps p = neighbors |> Seq.map (move p) |> Seq.filter (flip Set.contains ps)

    let step prev =
        let (removed,remaining) = prev |> Set.partition (fst >> (>) 4) |> tmap2 (Set.map snd) id
        let remainingPositions = remaining |> Set.map snd
        let effectMap = removed |> Set.toSeq |> Seq.collect (findNeighbors remainingPositions) |> Seq.countBy id |> Map
        let (changed, unchanged) = remaining |> Set.partition (snd >> (effectMap.ContainsKey))
        let changes = changed |> Set.map (fun (v,p) -> (v-effectMap[p],p))
        let next = Set.union changes unchanged
        match Set.count removed with
        | 0 -> None
        | x -> Some(x, next)

    let private bothParts = Seq.unfold step >> Seq.toList >> both List.head List.sum

    let solve: string seq -> string =
        parseInput >> bothParts >> output
