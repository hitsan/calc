package parser

object Combinator {
  import Node._

  def skipSpace[A](parser: Parser[A]): Parser[A] =
    code => parser(code.trim)

  def rep[A](parser: Parser[A]): Parser[List[A]] = code =>
    for {
      PResult(token, rest) <- parser(code)
      PResult(tokens, ret) <- rep(parser)(rest)
        .orElse(Option(PResult(List[A](), rest)))
    } yield PResult(token +: tokens, ret)

  def rep0[A](parser: Parser[A]): Parser[List[A]] =
    code => rep(parser)(code).orElse(Option(PResult(List[A](), code)))

  extension [A](parser: Parser[A]) def * = rep0(parser)

  def and[A, B](parsers: OrParser[A, B]*): Parser[List[A | B]] = code =>
    val initial = Option(PResult(List[A | B](), code))
    (initial /: parsers) { (acc, parser) =>
      for {
        PResult(tokens, code) <- acc
        case PResult[(A | B)](token, rest) <- parser(code)
      } yield PResult(tokens :+ token, rest)
    }

  extension [A, B](parser: OrParser[A, B])
    def &(parser2: OrParser[A, B]) = and(parser, parser2)

  def or[A](parsers: Parser[A]*): Parser[A] = code =>
    parsers.flatMap(parser => parser(code)).headOption

  extension [A](parser: Parser[A])
    def |(parser2: Parser[A]) = or(parser, parser2)

  def astRule[A <: List[_]](tokens: A): Node = {
    val initial = tokens.head match {
      case head: List[_] => astRule(head)
      case head: Ast     => head
    }
    (initial /: tokens.tail) { (ast, token) =>
      (ast, token) match {
        case (n: Node, l: List[_])    => astRule(n +: l)
        case (rhs: Node, op: TwoHand) => op(rhs)
        case (op: OneHand, lhs: Node) => op(lhs)
        case (_, _)                   => ast
      }
    } match {
      case n: Node => n
      case _       => IntNum(1)
    }
  }

  def exprRule[A <: List[_]](tokens: A): Node =
    (tokens.head, tokens(1), tokens.last) match {
      case (Achar('('), n:Node, Achar(')')) => n
    }

  extension [A](parser: Parser[A])
    def struct(f: A => Node): Parser[Node] = code =>
      for {
        PResult(tokens, rest) <- parser(code)
      } yield PResult(f(tokens), rest)
}
