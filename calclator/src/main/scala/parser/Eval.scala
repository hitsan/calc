package parser

object Eval {
  import Token._
  import Parser._
  import ParserGenerater._

  // Parser Expression
  // def term: Parser[Node] = code => ???
  // def factor[A]: Parser[List[Token | List[TwoHand | Token]]] = code =>
  //   and(unary, repeat0(and(operater('*'), unary)))(code)

  def unary: Parser[Token] = code => primary(code)
  def primary: Parser[Token] = code => intNum(code).orElse(anyString(code))
}
