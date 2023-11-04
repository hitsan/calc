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

  def repeat[T](allowZero: true)(parser: Parser[T]): Parser[List[T]] = code =>
    repeat(parser)(code).orElse(Option(PResult(List[T](), code)))

  def and[T](parsers: Parser[T]*): Parser[List[T]] = code => {
    val initial = Option(PResult(List[T](), code))
    (initial /: parsers) { (acc, parser) =>
      for {
        PResult(tokens, code) <- acc
        PResult(token, rest) <- parser(code)
      } yield PResult(tokens :+ token, rest)
    }
  }
  def or[T](parsers: Parser[T]*): Parser[T] =
    code => Option(parsers.flatMap(parser => parser(code)).head)

  def makeAst(tokens: List[Node]): Node = {
    (tokens.head /: tokens.tail) { (ast, token) =>
      (ast, token) match {
        case (rhs: Token, op: TwoHand) => op(rhs)
        case (op: OneHand, lhs: Token) => op(lhs)
      }
    }
  }

  def applyExpr(
      parser: Parser[List[Node]]
  )(f: List[Node] => Node): Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- parser(code)
    } yield PResult(f(tokens), rest)
}
