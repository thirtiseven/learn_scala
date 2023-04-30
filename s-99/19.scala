def rotate[A](n: Int, ls: List[A]): List[A] = {
  if (n >= 0) {
    ls.drop(n) ::: ls.take(n)
  } else {
    ls.takeRight(-n) ::: ls.dropRight(-n)
  }
}

print(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)))
print(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)))