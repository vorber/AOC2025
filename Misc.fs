namespace AOC

module Misc =
    let print msg x =
        printfn msg x
        x
    let output (p1,p2) = sprintf "(p1:%A, p2:%A)" p1 p2

    let tuple (arr:'a array) = (arr[0], arr[1])
    let both f g x = (f x, g x)
    let flip f a b = f b a
    let tmap f (a,b) = (f a, f b)

    let uncurry f (a, b) = f a b
    let curry f a b = f (a, b)

    let splitOn (split:string) (str:string) = str.Split(split)
    let multiple_count n lo hi = 
        let sh = (1+abs(lo/n))*n
        let l = lo + sh
        let h = hi + sh
        h/n - (l-1)/n
