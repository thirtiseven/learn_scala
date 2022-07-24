def reverse[A](ls: List[A]): List[A] = ls match {
  case h :: Nil  => List(h)
  case h :: tail => reverse(tail) :+ h
  case _         => throw new NoSuchElementException
}


def palindrome[A](ls: List[A]): Boolean = {
  ls == reverse(ls)
}

print(palindrome(List(1, 2, 3, 2, 1)))