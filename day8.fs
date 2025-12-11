namespace AOC

module Day8 =
    open Misc

    let input = [|
        "162,817,812";
        "57,618,57";
        "906,360,560";
        "592,479,940";
        "352,342,300";
        "466,668,158";
        "542,29,236";
        "431,825,988";
        "739,650,466";
        "52,470,668";
        "216,146,977";
        "819,987,18";
        "117,168,530";
        "805,96,715";
        "346,949,466";
        "970,615,88";
        "941,993,340";
        "862,61,35";
        "984,92,344";
        "425,690,689";
    |]

    [<StructuredFormatDisplay("Point({X},{Y},{Z})")>]
    type private Point =
        {X:int64; Y:int64; Z:int64}
        member this.distance2 (other: Point) =
            let dx = this.X - other.X
            let dy = this.Y - other.Y
            let dz = this.Z - other.Z
            dx*dx + dy*dy + dz*dz
        static member Create (x,y,z) = {X=x; Y=y; Z=z}

    let private parse input = 
        let parseLine = splitOn "," >> Array.map int >> triple >> Point.Create  
        let rec distinctPairs acc p arr =
            let last = (Array.length arr) - 1
            let pairs v = Array.map (curry id v) >> Array.toList
            if p=last then acc
            else distinctPairs (acc @ pairs arr[p] arr[p+1..]) (p+1) arr
        let points = input |> Seq.map parseLine |> Seq.toArray
        let distance p1 p2 = points[p1].distance2 points[p2]
        let indexes = points |> Array.mapi (curry fst)
        let edges = indexes |> distinctPairs [] 0 |> Seq.map (both (uncurry distance) id) |> Seq.sortBy fst |> Seq.map snd
        points, DisjointSet.Create(indexes), edges

    let private part1 (_, dsu:DisjointSet, edges) =
        let _ = edges |> Seq.take 1000 |> Seq.iter ((uncurry dsu.Union) >> Const ())
        dsu.Sizes |> Seq.sortDescending |> Seq.take 3 |> Seq.reduce (*)

    let private part2 (pts: Point array, dsu:DisjointSet, edges) =
        let consume last (p1, p2) = if dsu.Union p1 p2 then Some (p1,p2) else last
        edges
        |> Seq.fold consume None
        |> Option.map (fun e-> pts[fst e].X * pts[snd e].X)
        |> print "%A"

    let solve: string seq -> string =
        parse >> both part1 part2 >> output
    let test = input |> solve

