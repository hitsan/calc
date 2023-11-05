package parser

object ParserGenerater {
  import Token._

  def skipSpace[T](parser: Parser[T]): Parser[T] =
    code => parser(code.trim)

  def repeat[T](parser: Parser[T]): Parser[List[T]] = code =>
    for {
      PResult(token, rest) <- parser(code)
      PResult(tokens, ret) <- repeat(parser)(rest)
        .orElse(Option(PResult(List[T](), rest)))
    } yield PResult(token +: tokens, ret)

  def repeat0[T](parser: Parser[T]): Parser[List[T]] =
    code => repeat(parser)(code).orElse(Option(PResult(List[T](), code)))

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

  def makeAst(tokens: List[Node]): Node = {
    (tokens.head /: tokens.tail) { (ast, token) =>
      (ast, token) match {
        case (rhs: Token, op: TwoHand) => op(rhs)
        case (op: OneHand, lhs: Token) => op(lhs)
        case _                         => ast
      }
    }
  }

  def applyExpr(
      parser: Parser[List[Token | TwoHand]]
  )(f: List[Node] => Node): Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- parser(code)
    } yield PResult(f(tokens), rest)

  // [1, [[add, 1],[add, 1]]]
  // [1,[add, 1],[add, 1]]
  // [1, add, 1]
  // add(1,1)
  def makeAst1[A <: List[_]](tokens: A): Node = {
    val initial = tokens.head match {
      case a: Node => a
    }
    (initial /: tokens.tail) { (ast, token) =>
      (ast, token) match {
        case (n: Node, l: List[_])     => makeAst1(n +: l)
        case (rhs: Token, op: TwoHand) => op(rhs)
        case (op: OneHand, lhs: Token) => op(lhs)
        case (_, _)                    => ast
      }
    }
  }

  def applyExpr1[A](parser: Parser[A])(f: A => Node): Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- parser(code)
    } yield PResult(f(tokens), rest)
}
