namespace AOC

module Day2 =
    open AOC.Misc
    let input = 
        [|"11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"|] 

    let parse = splitOn "-" >> tuple >> tmap int64

    let isInvalid_p1 (s:string) = 
        match s.Length with
        | Odd _ -> false
        | Even x -> s[0..x/2-1] = s[x/2..]

    let isInvalid_p2 (s:string) = 
        let checkConcreteLength len = 
            match s.Length with
            | DivisibleBy len _ -> Seq.toArray >> Array.chunkBySize len >> Array.map System.String >> Array.forall ((=) s[0..len-1])
            | _ -> Const false
        let checkAll = Seq.filter (flip checkConcreteLength s) >> Seq.isEmpty >> not
        Seq.init (s.Length/2) ((+) 1)
        |> checkAll

    let invalidIds check (l:int64,h:int64) =
        Seq.init (h-l+1L |> int) (int64 >> ((+) l) >> string)
        |> Seq.filter check

    let sumInvalidIds invalidCheck = Seq.map (invalidIds invalidCheck) >> Seq.collect id >> Seq.sumBy int64

    let private part1 = sumInvalidIds isInvalid_p1

    let private part2 = sumInvalidIds isInvalid_p2

    let solve: string seq -> string = 
        Seq.head >> splitOn "," >> Seq.map parse
        >> both part1 part2 >> output
