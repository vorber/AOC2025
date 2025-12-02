namespace AOC

module Day2 =
    open AOC.Misc
    let input = 
        [|"11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"|] 

    let parse = splitOn "-" >> tuple >> tmap int64

    let isInvalid_p1 n = 
        let s = string n
        if s.Length % 2 <> 0 then false else
            let len = s.Length / 2
            s[0..len-1] = s[len..]

    let isInvalid_p2 (n:int64) = 
        let s = string n
        let checkConcreteLength len = 
            Seq.toArray 
            >> Array.chunkBySize len
            >> Array.map System.String 
            >> Array.forall (fun e -> e=s[0..len-1])
        let checkAll = 
            Seq.filter (fun len -> if s.Length % len <> 0 then false else checkConcreteLength len s)
            >> Seq.isEmpty 
            >> not
        if s.Length < 2 then false else
            Seq.init (s.Length/2) ((+) 1)
            |> checkAll

    let invalidIds check (l:int64,h:int64) =
        Seq.init (int (h-l+1L)) (fun i -> l + int64 i)
        |> Seq.filter check

    let sumInvalidIds invalidCheck = Seq.map (invalidIds invalidCheck) >> Seq.collect id >> Seq.sum

    let private part1 = sumInvalidIds isInvalid_p1

    let private part2 = sumInvalidIds isInvalid_p2

    let solve: string seq -> string = 
        Seq.head >> splitOn "," >> Seq.map parse
        >> both part1 part2 >> output
