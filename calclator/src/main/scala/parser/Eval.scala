package parser

object Eval {
  import Node._
  import Parser._

  // Parser Expression
  def term: Parser[Node] = code => ???
  // def factor: Parser[Node] = code =>
  //   chain(unary, repeat(true)(chain(operater('*'), unary)))

  def unary: Parser[Node] = code => primary(code)
  def primary: Parser[Node] = code => intNum(code).orElse(anyString(code))
}
