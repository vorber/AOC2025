namespace AOC

module Day3 =
    open Misc
    let input = [|
        "987654321111111";
        "811111111111119";
        "234234234234278";
        "818181911112111";
    |]

    let private findMaxJoltage (digits:int array) =
        let last = digits.Length - 1
        let findNext (pos, acc) skip =
            let segment = digits[pos..(last-skip)]
            let v = segment |> Array.max
            let p = segment |> Array.findIndex((=)v)
            (pos+p+1, acc*10L+int64 v)
        flip Seq.init id >> Seq.rev >> Seq.fold findNext (0,0L) >> snd

    let private overallMaxJoltage size = Array.map (flip findMaxJoltage size) >> Array.sum 
    let private part1 = overallMaxJoltage 2
    let private part2 = overallMaxJoltage 12

    let parse = Array.map (Seq.map charToInt >> Seq.toArray)

    let solve: string seq -> string = 
        Seq.toArray >> parse >> both part1 part2 >> output
