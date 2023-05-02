import scala.language.postfixOps

def combinations[A](n: Int, ls: List[A]): List[List[A]] = {
    def dfs(curid: Int, left: Int, cur: List[A]): List[List[A]] = {
        if (left == 0) List(cur)
        else if (curid == ls.length) Nil
        else dfs(curid + 1, left - 1, cur :+ ls(curid)) ::: dfs(curid + 1, left, cur)
    }
    dfs(0, n, Nil)
}

def group[A](siz: List[Int], ls: List[A]): List[List[List[A]]] = siz match {
    case Nil => List(Nil)
    case x :: xs => {
        combinations(x, ls) flatMap { c =>
            group(xs, ls diff c) map { c :: _ }
        }
    }
}

print(group(List(2, 2), List('a, 'b, 'c, 'd)))