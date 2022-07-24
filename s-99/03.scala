def kth[A](k: Int, ls: List[A]): A = (k, ls) match {
	case (0, h :: _) => h
	case (k, h :: tail) => kth(k-1, tail)
	case (_, Nil) => throw new NoSuchElementException
}

print(kth(2, List(1, 2, 3, 4)))