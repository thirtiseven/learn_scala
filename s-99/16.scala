def drop[A](n: Int, ls: List[A]): List[A] = {
    def dropR[A](c: Int, ls: List[A]): List[A] = (c, ls) match {
        case (_, Nil) => Nil
        case (1, _ :: tail) => dropR(n, tail)
        case (x, h :: tail) => h :: dropR(x - 1, tail)
    }
    dropR(n, ls)
}

print(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j)))