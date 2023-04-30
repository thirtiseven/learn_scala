def removeAt[A](k: Int, ls: List[A]): (List[A], A) = {
  (ls.take(k) ::: ls.drop(k + 1), ls(k))
}

print(removeAt(2, List('a, 'b, 'c, 'd)))