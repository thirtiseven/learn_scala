def duplicateN[A](n: Int, ls: List[A]): List[A] = {
  ls.flatMap(t => List.fill(n)(t))
}

print(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))