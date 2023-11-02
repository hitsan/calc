package parser

object Eval {
  import Ast.Node
  import Ast.Node._
  import Parser._

  // Parser Expression
  def term: Parser[NodeT] = code => ???
  // def factor: Parser[Node] = code =>
  //   chain(unary, repeat(true)(chain(operater('*'), unary)))

  def unary: Parser[NodeT] = code => primary(code)
  def primary: Parser[NodeT] = code => intNum(code).orElse(anyString(code))
}
