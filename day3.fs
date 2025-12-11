namespace AOC
#nowarn 57
module Day3 =
    open Misc
    let input = [|
        "987654321111111";
        "811111111111119";
        "234234234234278";
        "818181911112111";
    |]

    let private findMaxJoltage (digits:int array) =
        let findNext (pos, acc) skip =
            let nextPos = (+) (pos + 1)
            let nextAcc = int64 >> (+) (acc*10L)
            let next = Seq.indexed >> Seq.maxBy snd >> tmap2 nextPos nextAcc
            next digits[pos..^skip]
        flip Seq.init id >> Seq.rev >> Seq.fold findNext (0,0L) >> snd

    let private overallMaxJoltage size = Seq.sumBy (flip findMaxJoltage size)
    let private part1 = overallMaxJoltage 2
    let private part2 = overallMaxJoltage 12

    let parse = Seq.map (Seq.map charToInt >> Seq.toArray)

    let solve: string seq -> string = 
        parse >> both part1 part2 >> output

    let test = input |> solve
