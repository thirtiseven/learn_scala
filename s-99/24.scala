def lotto[A](n: Int, m: Int): List[Int] = {
    import scala.util.Random
    val r = new Random
    List.fill(n)(r.nextInt(m))
}