import scala.language.implicitConversions

class S99Int(val start: Int) {
    import S99Int._

    def isPrime: Boolean = {
        (start > 1) && (primes takeWhile { _ <= Math.sqrt(start) } forall { start % _ != 0 })
    }

    def isCoprimeTo(n: Int): Boolean = gcd(start, n) == 1

    def totient: Int = {
        (1 to start).filter(start.isCoprimeTo(_)).length
    }

    def primeFactors: List[Int] = {
        val sqrt = Math.sqrt(start).toInt
        (2 to sqrt).foldLeft(List[Int](), start) { (acc, i) =>
            val (factors, n) = acc
            def loop(n: Int, factors: List[Int]): (List[Int], Int) = {
                if (n % i == 0) loop(n / i, i :: factors)
                else (factors, n)
            }
            loop(n, factors)
        } match {
            case (factors, 1) => factors.reverse
            case (factors, n) => (n :: factors).reverse
        }
    }

    def primeFactorMultiplicity: Map[Int, Int] = {
        primeFactors.groupBy(identity).view.mapValues(_.length).toMap
    }

    def totientImproved: Int = {
        primeFactorMultiplicity.foldLeft(1) { (acc, i) =>
            val (p, m) = i
            acc * (p - 1) * Math.pow(p, m - 1).toInt
        }
    }

    def goldbach: (Int, Int) = {
        primes.takeWhile(_ < start).find(p => (start - p).isPrime) match {
            case None => throw new IllegalArgumentException
            case Some(p) => (p, start - p)
        }
    }
}

object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    val primes = LazyList.cons(2, LazyList.from(3, 2) filter { _.isPrime })

    def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

    def eulerSieve(n: Int): List[Int] = {
        def sieveRec(candidates: LazyList[Int], primes: List[Int]): List[Int] = {
            if (candidates.head > n) primes
            else sieveRec(candidates.tail filter (_ % candidates.head != 0), primes :+ candidates.head)
        }
        sieveRec(LazyList.from(2), Nil)
    }

    def listPrimesinRange(r: Range): List[Int] = {
        // euler sevie
        eulerSieve(r.max).dropWhile(_ < r.min).toList
    }

    def printGoldbachList(r: Range): Unit = {
        r.filter(_ % 2 == 0).foreach { i =>
            val (p1, p2) = i.goldbach
            println(s"$i = $p1 + $p2")
        }
    }

    def printGoldbachListLimited(r: Range, limit: Int): Unit = {
        r.filter(n => n > 2 && n % 2 == 0)
            .map(n => (n, n.goldbach))
            .filter(_._2._1 >= limit)
            .foreach {
                _ match { 
                    case (n, (p1, p2)) => println(s"$n = $p1 + $p2")
                }
            }
    }
}

import S99Int._
// println(7.isPrime)
// println(35.isCoprimeTo(64))
// println(10.totient)
// println(315.primeFactorMultiplicity)
// var now = System.currentTimeMillis()
// println(100090.totient)
// println("1: " + (System.currentTimeMillis() - now) + " ms.")
// now = System.currentTimeMillis()
// println(100090.totientImproved)
// println("2: " + (System.currentTimeMillis() - now) + " ms.")
println(listPrimesinRange(7 to 31))
println(eulerSieve(31))
println(28.goldbach)
printGoldbachList(9 to 20)
printGoldbachListLimited(1 to 2000, 50)
