def lsort[A](ls: List[List[A]]): List[List[A]] = {
    ls.sortBy(_.length)
}

def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
    val freqs = Map() ++ (ls map { e => (e, ls.count(_ == e)) })
    ls.sortBy(e => freqs(e))
}

println(lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))

println(lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))