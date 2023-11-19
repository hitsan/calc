package parser

object Expression {
  import Node._
  import Primitive._
  import Combinator._

  // Parser Expression
  def expression: Parser[Node] = code => comparison(code)

  def comparison: Parser[Node] = code =>
    (term & ((greaterEqual | greater | lessEqual | less) & term).*)
      .struct(astRule)(code)

  def term: Parser[Node] = code =>
    (factor & ((plus | minus) & factor).*).struct(astRule)(code)

  def factor: Parser[Node] = code =>
    (unary & ((times | divide) & unary).*).struct(astRule)(code)

  def unary: Parser[Node] = code =>
    ((bang | negative) & unary).struct(unaryRule)(code).orElse(primary(code))

  def primary: Parser[Node] = code =>
    (intNum | bool | anyString | parenthesesExpr)(code)

  def parenthesesExpr: Parser[Node] = code =>
    and(lParentheses, expression, rParentheses).struct(parenthesesRule)(code)

  extension [A](parser: Parser[A])
    def struct(f: A => Node): Parser[Node] = code =>
      parser(code).map { case PResult(tokens, rest) =>
        PResult(f(tokens), rest)
      }

  def astRule[A <: List[_]](tokens: A): Node = {
    val initial = tokens.head match {
      case head: List[_] => astRule(head)
      case head: Ast     => head
    }
    tokens.tail
      .foldLeft(initial) { (ast, token) =>
        (ast, token) match {
          case (n: Node, l: List[_])    => astRule(n +: l)
          case (rhs: Node, op: TwoHand) => op(rhs)
          case (op: OneHand, lhs: Node) => op(lhs)
          case (_, _)                   => ast
        }
      }
      .asInstanceOf[Node]
  }

  def unaryRule[A <: List[_]](tokens: A): Node =
    (tokens.head, tokens.last) match {
      case (op: OneHand, rhs: Node) => op(rhs)
      case (_, _)                   => sys.error("Invalid token")
    }

  def parenthesesRule[A <: List[_]](tokens: A): Node =
    (tokens.head, tokens(1), tokens.last) match {
      case (LParentheses, n: Node, RParentheses) => n
    }
}
