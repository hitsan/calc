package parser

object ParserGenerater {
  import Token._

  def skipSpace[T](parser: Parser[T]): Parser[T] =
    code => parser(code.trim)

  def rep[T](parser: Parser[T]): Parser[List[T]] = code =>
    for {
      PResult(token, rest) <- parser(code)
      PResult(tokens, ret) <- rep(parser)(rest)
        .orElse(Option(PResult(List[T](), rest)))
    } yield PResult(token +: tokens, ret)

  def rep0[T](parser: Parser[T]): Parser[List[T]] =
    code => rep(parser)(code).orElse(Option(PResult(List[T](), code)))

  def and[A, B](parsers: OrParser[A, B]*): Parser[List[A | B]] = code =>
    val initial = Option(PResult(List[A | B](), code))
    (initial /: parsers) { (acc, parser) =>
      for {
        PResult(tokens, code) <- acc
        case PResult[(A | B)](token, rest) <- parser(code)
      } yield PResult(tokens :+ token, rest)
    }

  def or[A](parsers: Parser[A]*): Parser[A] = code =>
    parsers.flatMap(parser => parser(code)).headOption

  def makeAst[A <: List[_]](tokens: A): Node = {
    val initial = tokens.head match {
      case head: List[_] => makeAst(head)
      case head: Node    => head
    }
    (initial /: tokens.tail) { (ast, token) =>
      (ast, token) match {
        case (n: Node, l: List[_])     => makeAst(n +: l)
        case (rhs: Token, op: TwoHand) => op(rhs)
        case (op: OneHand, lhs: Token) => op(lhs)
        case (_, _)                    => ast
      }
    }
  }

  def applyExpr[A](parser: Parser[A])(f: A => Node): Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- parser(code)
    } yield PResult(f(tokens), rest)
}
