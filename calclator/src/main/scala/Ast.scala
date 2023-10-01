object Ast {
  enum Node {
    case Add(lhs: Node, rhs: Node)
    case Sub(lhs: Node, rhs: Node)
    case Mul(lhs: Node, rhs: Node)
    case Div(lhs: Node, rhs: Node)
    case Integer(n: Int)
  }
}
