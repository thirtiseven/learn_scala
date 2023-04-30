def encodeDirect(ls: List[Symbol]): List[(Int, Symbol)] = {
    if (ls.isEmpty) {
        List()
    } else {
        val (packed, next) = ls.span( _ == ls.head)
        (packed.length, packed.head) :: encodeDirect(next)
    }
}

print(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))