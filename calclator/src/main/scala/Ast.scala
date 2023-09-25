object Ast {
  case class Node(
      nodeType: NodeType,
      lhs: Node,
      rhs: Node
  )
  enum NodeType {
    case Add
    case Sub
    case Mul
    case Div
    case Num(n: Int)
  }
}
