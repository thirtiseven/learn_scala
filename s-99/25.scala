def removeAt[A](k: Int, ls: List[A]): (List[A], A) = {
  (ls.take(k) ::: ls.drop(k + 1), ls(k))
}

def randomSelect[A](ls: List[A], n: Int): List[A] = {
  def randomSelectR[A](ls: List[A], n: Int, r: util.Random): List[A] = {
    if (n <= 0) Nil
    else {
      val (rest, e) = removeAt(r.nextInt(ls.length), ls)
      e :: randomSelectR(rest, n - 1, r)
    }
  }
  randomSelectR(ls, n, new util.Random)
}

def randomPermute[A](ls: List[A]): List[A] = randomSelect(ls, ls.length)

print(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))