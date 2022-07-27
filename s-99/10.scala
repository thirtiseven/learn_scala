//def pack[A](ls: List[A]): List[List[A]] = {
//if (ls.isEmpty) List(List())
//else {
//  val (packed, next) = ls span { _ == ls.head }
//  if (next == Nil) List(packed)
//  else packed :: pack(next)
//}
//}

def pack[A](ls: List[A]): List[List[A]] = ls match {
  case Nil => List(List())
  case ls => {
    val (packed, next) = ls.span( _ == ls.head)
    next match {
      case Nil => List(packed)
      case next => packed :: pack(next)
    }
  }
}

def encode_pack[A](ls: List[List[A]]): List[(Int, A)] = {
  ls.map(lls => (lls.length, lls(0)))
}

def encode[A](ls: List[A]): List[(Int, A)] = encode_pack(pack(ls))

print(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

