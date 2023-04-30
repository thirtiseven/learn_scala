def slice[A](start: Int, end: Int, list: List[A]): List[A] = {
  list.drop(start).take(end - start)
}

print(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j)))