def reverse[A](ls: List[A]): List[A] = ls match {
    case h :: Nil  => List(h)
    case h :: tail => reverse(tail) :+ h
    case _         => throw new NoSuchElementException
}

def isPalindrome[A](ls: List[A]): Boolean = ls match {
    ls == reverse(ls)
}

