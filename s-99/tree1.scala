sealed abstract class Tree[+T] {
  def isMirrorOf[V](tree: Tree[V]): Boolean
  def isSymmetric: Boolean
  def addValue[U >: T](x: U)(implicit ev: U => Ordered[U]): Tree[U]
  def nodeCount: Int
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
  def nodeCount: Int = 1 + left.nodeCount + right.nodeCount

  def leafCount: Int = (left, right) match {
    case (End, End) => 1
    case _ => left.leafCount + right.leafCount
  }

  def leafList: List[T] = (left, right) match {
    case (End, End) => List(value)
    case _ => left.leafList ::: right.leafList
  }

  def internalList: List[T] = (left, right) match {
    case (End, End) => Nil
    case _ => value :: left.internalList ::: right.internalList
  }

  def atLevel(level: Int): List[T] = {
    if (level < 1) Nil
    else if (level == 1) List(value)
    else left.atLevel(level - 1) ::: right.atLevel(level - 1)
  }

}

case object End extends Tree[Nothing] {
  override def toString = "."
  def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End
  def isSymmetric: Boolean = true
  def addValue[U](x: U)(implicit ev: U => Ordered[U]): Tree[U] = Node(x)
  def nodeCount: Int = 0
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
      fullHeight.flatMap((f) => shortHeight.flatMap((s) => List(Node(value, f, s), Node(value, s, f))))
    }
  }

  def minHbalNodes(height: Int): Int = height match {
    case n if n < 1 => 0
    case 1 => 1
    case n => minHbalNodes(n - 1) + minHbalNodes(n - 2) + 1
  }

  def maxHbalNodes(height: Int): Int = {
    math.pow(2, height).toInt - 1
  }

  def minHbalHeight(nodes: Int): Int = {
    if (nodes == 0) 0
    else minHbalHeight(nodes / 2) + 1
  }

  def maxHbalHeight(nodes: Int): Int = {
    Stream.from(1).takeWhile(minHbalNodes(_) <= nodes).last
  }

  def hbalTreesWithNodes[T](nodes: Int, value: T): List[Tree[T]] = {
    (minHbalHeight(nodes) to maxHbalHeight(nodes)).flatMap(hbalTrees(_, value)).filter(_.nodeCount == nodes).toList
  }

}

// println(Tree.cBalanced(4, "x"))
// println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).isSymmetric)
// println(Node('a', Node('b'), Node('c')).isSymmetric)
// val res0 = End.addValue(2)
// val res1 = res0.addValue(3)
// val res2 = res1.addValue(0)
// val res3 = Tree.fromList(List(3, 2, 5, 7, 1))
// println(res3)

val res4 = Tree.hbalTreesWithNodes(4, "x")
println(res4)