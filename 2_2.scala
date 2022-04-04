def findFirst[A](as: Array[A], p: A => Boolean): Int = {
	@annotation.tailrec
	def loop(n: Int): Int = {
		if (n >= as.length) {
			-1
		} else if (p(as(n))) {
			n
		} else {
			loop(n+1)
		}
	}
	loop(0)
}

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
	var res = true
	for (i <- 1 until as.length) {
		if (ordered(as(i-1), as(i)) == false) {
			res = false
		}
	}
	return res
}


//val x = findFirst[String](Array("ab", "bc"), (x: String) => x == "bc")
val x = isSorted[Int](Array(1, 2, 3, 8, 5, 6), (a: Int, b: Int) => a < b)

println(x)