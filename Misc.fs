namespace AOC

module Misc =
    let print msg x =
        printfn msg x
        x

    let tuple (arr:'a array) = (arr[0], arr[1])
    let both f g x = (f x, g x)
    let flip f a b = f b a

    let multiple_count n lo hi = 
        let sh = (1+abs(lo/n))*n
        let l = lo + sh
        let h = hi + sh
        h/n - (l-1)/n
