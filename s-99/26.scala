def combinations[A](n: Int, ls: List[A]): List[List[A]] = {
    def dfs(curid: Int, left: Int, cur: List[A]): List[List[A]] = {
        if (left == 0) List(cur)
        else if (curid == ls.length) Nil
        else dfs(curid + 1, left - 1, cur :+ ls(curid)) ::: dfs(curid + 1, left, cur)
    }
    dfs(0, n, Nil)
}

print(combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)))