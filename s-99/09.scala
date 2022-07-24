def pack[A](ls: List[A]): List[List[A]] = {
  if (ls.isEmpty) List(List())
  else {
    val (packed, next) = ls span { _ == ls.head }
    if (next == Nil) List(packed)
    else packed :: pack(next)
  }
}

def pack2[A](ls: List[A]): List[List[A]] = ls match {
  case Nil => List(List())
  case ls => {
    val (packed, next) = ls.span(_==ls.head)
    next match {
      case Nil => List(packed)
      case next => packed :: pack(next)
    }
  }
}

print(pack2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))