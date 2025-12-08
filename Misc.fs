namespace AOC

module Misc =
    open System.Collections.Generic
    let print msg x =
        printfn msg x
        x
    let output (p1,p2) = sprintf "(p1:%A, p2:%A)" p1 p2
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

    let Const a _ = a

    let (<&>) f g x = f x && g x

    let (|Even|Odd|) n = if n % 2 = 0 then Even n else Odd n
    let (|DivisibleBy|_|) x n = if n % x = 0 then  Some(n) else None

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
