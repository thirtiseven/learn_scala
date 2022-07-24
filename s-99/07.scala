def flatten(ls: List[Any]): List[Any] = ls flatMap {
  case ms: List[Any]  => flatten(ms)
  case x => List(x)
}

print(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))