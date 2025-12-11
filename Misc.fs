namespace AOC

module Misc =
    open System.Collections.Generic
    open System.Diagnostics

    let print msg x =
        printfn msg x
        x
    let output (p1,p2) = sprintf "(p1:%A, p2:%A)" p1 p2
    let outputTimed (((p1, t1), (p2, t2)), overall) = sprintf $"p1:%A{p1} ({t1}), p2:%A{p2} ({t2}), overall: {overall}" 
    let timed f a =
        let timer = Stopwatch.StartNew()
        let result = f a
        result, (sprintf "%i ms" timer.ElapsedMilliseconds)

    let charToInt (c:char) = int c - int '0'

    let tuple (arr:IList<'a>) = (arr[0], arr[1])
    let triple (arr:'a array) = (arr[0], arr[1], arr[2])
    let inline both f g x = (f x, g x)
    let inline flip f a b = f b a
    let tmap f (a,b) = (f a, f b)
    let tmap2 f g (a, b) = (f a, g b)

    let uncurry f (a, b) = f a b
    let curry f a b = f (a, b)

    let dup x = (x,x)

    let splitOn (split:string) (str:string) = str.Split(split)
    let splitWhen (predicate: 'a -> bool) (elements: 'a list) =
        let split l (a, t) =
            if predicate l then ([], a::t)
            else (l::a, t)
        List.foldBack split elements ([], []) |> fun (a, t) -> a::t

    let multiple_count n lo hi = 
        let sh = (1+abs(lo/n))*n
        let l = lo + sh
        let h = hi + sh
        h/n - (l-1)/n
    
    let combinations<'a>: 'a array -> ('a*'a) list =
        let rec distinctPairs acc p arr =
            let last = (Array.length arr) - 1
            let pairs v = Array.map (curry id v) >> Array.toList
            if p=last then acc
            else distinctPairs (acc @ pairs arr[p] arr[p+1..]) (p+1) arr
        distinctPairs [] 0

    let Const a _ = a

    let (<&>) f g x = f x && g x

    let (|Even|Odd|) n = if n % 2 = 0 then Even n else Odd n
    let (|DivisibleBy|_|) x n = if n % x = 0 then  Some(n) else None

    
    module Set =
        let isNonEmptySubset s1 s2 = not (Set.isEmpty s1) && Set.isSubset s1 s2 
        let (|Empty|NonEmpty|) s = if Set.isEmpty s then Empty else NonEmpty s

    module Map =
        let tryFind2 k1 k2 = Map.tryFind k1  >> Option.bind (Map.tryFind k2)

    type DisjointSet(parents: int array, ranks: int array, sizes: int array) =
        let mutable parents = parents
        let mutable ranks = ranks
        let mutable sizes = sizes

        static member Create(items: int array) =
            let p = items |> Array.copy
            let r = Array.init p.Length (Const 0)
            let s = Array.init p.Length (Const 1)
            DisjointSet(p,r,s)

        member this.Find item =
            if parents[item] = item then item
            else
                let root = this.Find parents[item]
                Array.set parents item root
                root

        member this.Union a b =
            let a = this.Find a
            let b = this.Find b
            if a <> b then
                let ar = ranks[a]
                let br = ranks[b]
                let sz = sizes[a]+sizes[b]
                if ar < br then
                    Array.set parents a b
                    Array.set sizes b sz
                else 
                    Array.set parents b a
                    Array.set sizes a sz
                    if ar = br then Array.set ranks a (ar+1)
                true
            else false
        
        member _.Sizes =
            parents
            |> Array.indexed
            |> Array.filter (uncurry (=))
            |> Array.map fst
            |> Array.map (fun i -> sizes[i])

    type BoundingBox =
        {L:int;R:int;T:int;B:int}
        static member FromRect r = {
            L = tmap fst r ||> min;
            R = tmap fst r ||> max;
            T = tmap snd r ||> min;
            B = tmap snd r ||> max;
        }
        member this.intersectsInsideWith other =
            let left = this.L >= other.R
            let right = this.R <= other.L
            let above = this.B <= other.T
            let below = this.T >= other.B
            not (left || right || above || below)
        static member intersectsInside (a:BoundingBox) (b:BoundingBox) = a.intersectsInsideWith b

