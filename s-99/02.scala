def last_but_one(ls: List[Int], a:Int = 0): Int = ls match {
	case h :: Nil => a
	case h :: tail => last_but_one(tail, h)
	case Nil => -1
}

def last_but_one_2(ls: List[Int]): Int = ls match {
	case h :: _ :: Nil => h
	case h :: tail => last_but_one(tail)
	case Nil => -1
}

print(last_but_one_2((List(1, 1, 2, 3, 5, 8))))