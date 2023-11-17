package parser

object Expression {
  import Node._
  import Primitive._
  import Combinator._

  // Parser Expression
  def expression: Parser[Node] = code => term(code)
  def term: Parser[Node] = code =>
    (factor & ((plus | minus) & factor).*).struct(astRule)(code)

  def factor: Parser[Node] = code =>
    (unary & ((times | divide) & unary).*).struct(astRule)(code)

  def unary: Parser[Node] = code => (((bang | negative) & unary) | primary).struct(astRule)(code)
  def primary: Parser[Node] = code =>
    (intNum | bool | anyString | parenthesesExpr)(code)

  def parenthesesExpr: Parser[Node] = code =>
    and(char('('), expression, char(')')).struct(exprRule)(code)
}
