def length[A](ls: List[A]): Int = ls match {
  case h :: Nil  => 1
  case _ :: tail => 1 + length(tail)
  case _         => throw new NoSuchElementException
}

print(length(List(1, 2, 3, 4)))
