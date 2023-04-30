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

def encode_pack[A](ls: List[List[A]]): List[Any] = {
  ls.map(lls => lls.length match {
    case 1 => lls(0)
    case _ => (lls.length, lls(0))
  })
}

def encodeModified[A](ls: List[A]): List[Any] = encode_pack(pack(ls))

print(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

