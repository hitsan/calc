object Ast {
  enum Expression {
    case Add(lhs: Expression, rhs: Expression)
    case Sub(lhs: Expression, rhs: Expression)
    case Mul(lhs: Expression, rhs: Expression)
    case Div(lhs: Expression, rhs: Expression)
    case Operator(op: Char)
    case Integer(n: Int)
    case Id(id: String)
  }
}
