package parser

object Expression {
  import Node._
  import Primitive._
  import Combinator._

  // Parser Expression
  def expression: Parser[Node] = term
  def term: Parser[Node] =
    (factor & ((plus | minus) & factor).*).struct(astRule)

  def factor: Parser[Node] =
    (unary & ((times | divide) & unary).*).struct(astRule)

  def unary: Parser[Node] = primary
  def primary: Parser[Node] = {
    or(intNum, anyString, dE)
  }

  def dE: Parser[Node] = and(char('('), expression, char(')')).struct(exprRule)
}
