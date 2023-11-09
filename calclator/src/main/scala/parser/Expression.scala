package parser

object Expression {
  import Token._
  import Primitive._
  import Combinator._

  // Parser Expression
  def term: Parser[Node] =
    applyExpr(and(factor, rep0(and(or(operater('+'), operater('-')), factor))))(
      makeAst
    )

  def factor: Parser[Node] =
    applyExpr(and(unary, rep0(and(or(operater('*'), operater('/')), unary))))(
      makeAst
    )

  def unary: Parser[Token] = primary
  def primary: Parser[Token] = or(intNum, anyString)
}
