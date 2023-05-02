import scala.language.implicitConversions

class S99Int(val start: Int) {
    import S99Int._
    def isPrime: Boolean = {
        (start > 1) && (primes takeWhile { _ <= Math.sqrt(start) } forall { start % _ != 0 })
    }
    def isCoprimeTo(n: Int): Boolean = gcd(start, n) == 1
}

object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
    val primes = LazyList.cons(2, LazyList.from(3, 2) filter { _.isPrime })
    def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)
}

import S99Int._
println(7.isPrime)
println(35.isCoprimeTo(64))