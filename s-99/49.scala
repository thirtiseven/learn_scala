def gray(n: Int): List[String] = n match {
    case 0 => List("")
    case n => gray(n - 1).map("0" + _) ::: gray(n - 1).reverse.map("1" + _)
}

import scala.collection.mutable

private val strings = mutable.Map(0 -> List(""))

def grayMemoized(n: Int): List[String] = {
    if (!strings.contains(n)) {
        strings + (n -> (grayMemoized(n - 1).map("0" + _) ::: grayMemoized(n - 1).reverse.map("1" + _)))
    }
    strings(n)
}


println(gray(3))

println(grayMemoized(3))