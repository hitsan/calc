package parser

object Expression {
  import Node._
  import Primitive._
  import Combinator._

  // Parser Expression
  def expression: Parser[Node] = ???
  def term: Parser[Node] =
    and(factor, rep0(and(or(operater('+'), operater('-')), factor)))
      .struct(exprRule)

  def factor: Parser[Node] =
    and(unary, rep0(and(or(operater('*'), operater('/')), unary)))
      .struct(exprRule)

  def unary: Parser[Node] = primary
  def primary: Parser[Node] = or(intNum, anyString)
}
