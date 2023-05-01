def range(start: Int, end: Int): List[Int] = (start, end) match {
    case (s, e) if s == e => List(s)
    case (s, e) if s < e => s :: range(s + 1, e)
}

def rangeTail(start: Int, end: Int): List[Int] = {
    def rangeR(end: Int, result: List[Int]): List[Int] = {
        if (end < start) result
        else rangeR(end - 1, end :: result)
    }
    rangeR(end, Nil)
}

print(range(4, 9))
print(rangeTail(4, 9))