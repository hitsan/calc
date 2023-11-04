package parser

object Eval {
  import Token._
  import Parser._

  // Parser Expression
  // def term: Parser[Node] = code => ???
  // def factor: Parser[Node] = code =>
  //   chain(unary, repeat(true)(chain(operater('*'), unary)))

  // def unary: Parser[Node] = code => primary(code)
  def primary: Parser[Token] = code => intNum(code).orElse(anyString(code))
}
