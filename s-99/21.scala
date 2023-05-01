def insertAt[A](n: A, k: Int, ls: List[A]): List[A] = {
  ls.take(k) ::: n :: ls.drop(k)
}

print(insertAt('new, 1, List('a, 'b, 'c, 'd)))