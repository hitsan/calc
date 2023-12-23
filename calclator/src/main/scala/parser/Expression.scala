package parser

object Expression {
  import Node._
  import Primitive._
  import Combinator._

  def program: Parser[Node] = code => declaration(code)

  def declaration: Parser[Node] = code => (varDecl | statement)(code)

  def varDecl: Parser[Node] = code =>
    and(varKey, identifier, assign, expression, semicolon)
      .struct(varDeclRule)(code)

  def statement: Parser[Node] = code => exprStmt(code)

  def exprStmt: Parser[Node] = code =>
    (expression & semicolon).struct(exprStmtRule)(code)

  def expression: Parser[Node] = code => equality(code)

  def equality: Parser[Node] = code =>
    (comparison & ((notEqual | equal) & comparison).*)
      .struct(astRule)(code)

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

  def varDeclRule[A <: List[_]](tokens: A): Node =
    tokens match {
      case Var :: (name: Identifier) :: Assign :: (expr: Node) :: Semicolon :: Nil =>
        VarDecl(name, expr)
    }

  def exprStmtRule[A <: List[_]](tokens: A): Node =
    tokens.head.asInstanceOf[Node]

  def astRule[A <: List[_]](tokens: A): Node = {
    def flatTokens(tokens: List[_]): List[_] = tokens.flatMap {
      case l: List[_] => flatTokens(l)
      case a          => List(a)
    }
    val toks = flatTokens(tokens)
    toks.tail
      .foldLeft(toks.head) { (ast, token) =>
        (ast, token) match {
          case (rhs: Node, op: TwoHand) => op(rhs)
          case (op: OneHand, lhs: Node) => op(lhs)
        }
      }
      .asInstanceOf[Node]
  }

  def unaryRule[A <: List[_]](tokens: A): Node =
    (tokens.head, tokens.last) match {
      case (op: OneHand, rhs: Node) => op(rhs)
    }

  def parenthesesRule[A <: List[_]](tokens: A): Node =
    (tokens.head, tokens(1), tokens.last) match {
      case (LParentheses, n: Node, RParentheses) => n
    }
}
