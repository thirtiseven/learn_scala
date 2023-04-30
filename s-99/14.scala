def duplicate[A](ls: List[A]): List[A] = {
  ls.flatMap(t => List.fill(2)(t))
}

print(duplicate(List('a, 'b, 'c, 'c, 'd)))