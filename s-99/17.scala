def split[A](n: Int, ls: List[A]): Tuple2[List[A], List[A]] = {
    ls.zipWithIndex.span(t => t._2 < n) match {
        case (l1, l2) => (l1.map(t => t._1), l2.map(t => t._1))
    }
}

print(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j)))