namespace AOC

module Day11 =
    open Misc
    open Graph

    let p1input = [|
        "aaa: you hhh";
        "you: bbb ccc";
        "bbb: ddd eee";
        "ccc: ddd eee fff";
        "ddd: ggg";
        "eee: out";
        "fff: out";
        "ggg: out";
        "hhh: ccc fff iii";
        "iii: out";
    |]

    let p2input = [|
        "svr: aaa bbb";
        "aaa: fft";
        "fft: ccc";
        "bbb: tty";
        "tty: ccc";
        "ccc: ddd eee";
        "ddd: hub";
        "hub: fff";
        "eee: dac";
        "dac: fff";
        "fff: ggg hhh";
        "ggg: out";
        "hhh: out";
    |]

    let private parse =
        let parseLine = splitOn ": " >> tuple >> tmap2 id (splitOn " " >> Seq.map (both id (Const 1)))
        let makeGraph = both (Seq.map fst >> Graph.fromLabels >> Graph.addVertex "out") id >> uncurry (Seq.fold Graph.addEdges)
        Seq.map parseLine >> makeGraph >> both id _.topologicalSort

    type G = Graph<string>
    let private countPaths src dst (graph:G) sorted =
        let rec walk pc vs =
            let addPaths count values key = values |> Map.change key (fun p -> Some (count + Option.defaultValue 0L p)) 
            match vs with
            | [] -> pc
            | v::t ->
                let pathCount = pc |> Map.find v
                let pc' = graph.OutgoingEdges[v].Keys |> Seq.fold (addPaths pathCount) pc
                walk pc' t
        let initialPaths = graph.Vertices |> Seq.map (fun v -> if v = src then v,1L else v,0L) |> Map
        walk initialPaths sorted |> Map.find dst

    let private part1 (g:G, sorted) = countPaths "you" "out" g sorted

    let private part2 (g:G, sorted) =
        let count src dst = countPaths src dst g sorted
        (count "svr" "fft") * (count "fft" "dac") * (count "dac" "out")
        + 
        (count "svr" "dac") * (count "dac" "fft") * (count "fft" "out")

    let solve: string seq -> string =
        timed (parse >> both (timed part1) (timed part2)) >> outputTimed

    let test =
        (p1input |> parse |> part1, p2input |> parse |> part2) |> output

