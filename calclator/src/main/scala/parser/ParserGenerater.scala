package parser

object ParserGenerater {
  import Token._

  def skipSpace(parser: Parser[Node]): Parser[Node] =
    code => parser(code.trim)

  def repeat(parser: Parser[Node]): Parser[List[Node]] = code =>
    for {
      PResult(token, rest) <- parser(code)
      PResult(tokens, ret) <- repeat(parser)(rest)
        .orElse(Option(PResult(List[Node](), rest)))
    } yield PResult(token +: tokens, ret)

  def repeat(allowZero: true)(parser: Parser[Node]): Parser[List[Node]] = code =>
    repeat(parser)(code).orElse(Option(PResult(List[Node](), code)))

  def and(parsers: Parser[Node]*): Parser[List[Node]] = code => {
    val initial = Option(PResult(List[Node](), code))
    (initial /: parsers) { (acc, parser) =>
      for {
        PResult(tokens, code) <- acc
        PResult(token, rest) <- parser(code)
      } yield PResult(tokens :+ token, rest)
    }
  }
  def or(parsers: Parser[Node]*): Parser[Node] =
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
