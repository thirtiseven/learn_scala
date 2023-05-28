sealed abstract class Tree[+T] {
  def isMirrorOf[V](tree: Tree[V]): Boolean
  def isSymmetric: Boolean
  def addValue[U >: T](x: U)(implicit ev: U => Ordered[U]): Tree[U]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString =
    "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
    case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
    case _ => false
  }
  def isSymmetric: Boolean = left.isMirrorOf(right)
  def addValue[U >: T](x: U)(implicit ev: U => Ordered[U]): Tree[U] = {
    if (x < value) Node(value, left.addValue(x), right)
    else Node(value, left, right.addValue(x))
  }
}

case object End extends Tree[Nothing] {
  override def toString = "."
  def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End
  def isSymmetric: Boolean = true
  def addValue[U](x: U)(implicit ev: U => Ordered[U]): Tree[U] = Node(x)
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {

  def cBalanced[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
    case n if n < 1 => List(End)
    case n if n % 2 == 1 => {
      val subtrees = cBalanced(n / 2, value)
      subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
    }
    case n if n % 2 == 0 => {
      val lesserSubtrees = cBalanced((n - 1) / 2, value)
      val greaterSubtrees = cBalanced((n - 1) / 2 + 1, value)
      lesserSubtrees.flatMap(l => greaterSubtrees.flatMap(g => List(Node(value, l, g), Node(value, g, l))))
    }
  }

  def fromList[T](ls: List[T])(implicit ev: T => Ordered[T]): Tree[T] = {
    ls.foldLeft(End: Tree[T])((t, x) => t.addValue(x))
  }

  def symmetricBalancedTrees[T](nodes: Int, value: T): List[Tree[T]] = {
    cBalanced(nodes, value).filter(_.isSymmetric)
  }

  def hbalTrees[T](height: Int, value: T): List[Tree[T]] = height match {
    case n if n < 1 => List(End)
    case 1 => List(Node(value))
    case n => {
      val fullHeight = hbalTrees(n - 1, value)
      val shortHeight = hbalTrees(n - 2, value)
      fullHeight.flatMap((l) => fullHeight.map((r) => Node(value, l, r))) :::
      fullHeight.flatMap((f) => short.flatMap((s) => List(Node(value, f, s), Node(value, s, f))))
    }
  }

}

// println(Tree.cBalanced(4, "x"))
// println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).isSymmetric)
// println(Node('a', Node('b'), Node('c')).isSymmetric)
val res0 = End.addValue(2)
val res1 = res0.addValue(3)
val res2 = res1.addValue(0)
val res3 = Tree.fromList(List(3, 2, 5, 7, 1))
println(res3)