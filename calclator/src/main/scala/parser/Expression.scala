package parser

object Expression {
  import Token._
  import Primitive._
  import Combinator._

  // Parser Expression
  def expression: Parser[Node] = ???
  def term: Parser[Node] =
    and(factor, rep0(and(or(operater('+'), operater('-')), factor)))
      .applyExpr(makeAst)

  def factor: Parser[Node] =
    and(unary, rep0(and(or(operater('*'), operater('/')), unary)))
      .applyExpr(makeAst)

  def unary: Parser[Token] = primary
  def primary: Parser[Token] = or(intNum, anyString)
}
