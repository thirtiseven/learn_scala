// Standard recursive.
def compressRecursive[A](ls: List[A]): List[A] = ls match {
  case Nil       => Nil
  case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
}

// Tail recursive.
def compressTailRecursive[A](ls: List[A]): List[A] = {
  def compressR(result: List[A], curList: List[A]): List[A] = curList match {
    case h :: tail => compressR(h :: result, tail.dropWhile(_ == h))
    case Nil       => result.reverse
  }
  compressR(Nil, ls)
}

// Functional.
def compressFunctional[A](ls: List[A]): List[A] =
  ls.foldRight(List[A]()) { (h, r) =>
    if (r.isEmpty || r.head != h) h :: r
    else r
  }

compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))