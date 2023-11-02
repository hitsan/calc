package parser

object Ast {
  enum Node {
    case Add(lhs: Node)(rhs: Node)
    case Sub(lhs: Node)(rhs: Node)
    case Mul(lhs: Node)(rhs: Node)
    case Div(lhs: Node)(rhs: Node)
    case IntNum(n: Int)
    case Str(id: String)
    case CharX(c: Char)
    case Dummy
  }
}