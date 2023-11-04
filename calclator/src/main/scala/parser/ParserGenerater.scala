package parser

object ParserGenerater {
  import Node._

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

  def chain[A](parsers: Parser[A]*): Parser[List[A]] = code => {
    val initial = Option(PResult(List[A](), code))
    (initial /: parsers) { (acc, parser) =>
      for {
        PResult(tokens, code) <- acc
        PResult(token, rest) <- parser(code)
      } yield PResult(tokens :+ token, rest)
    }
  }

  // def comb(op: Operater)(node: Node): OneHadNode = op(node)
  // def comb1(node: Node)(op: OneHadNode): Node = op(node)
  // def comb2(rhs: OneHadNode)(node2: OneHadNode): OneHadNode = lhs => node2(rhs)(lhs)
  // AND(Binary)
  // def and[A, B, C](f: A => B => C)(cur: Parser[A], next: Parser[B]): Parser[C] =
  //   code =>
  //     for {
  //       PResult(curToken, curRest) <- cur(code)
  //       PResult(nextToken, nextRest) <- next(curRest)
  //     } yield PResult(f(curToken)(nextToken), nextRest)

  def makeAst(tokens: List[Ast[Node]]): Ast[Node] = {
    (tokens.head /: tokens.tail) { (ast, token) =>
      (ast, token) match {
        case (rhs: Node, op: TwoHand) => op(rhs)
        case (op: OneHand, lhs: Node) => op(lhs)
      }
    }
  }

  // OR
  def or[T](parsers: Parser[T]*): Parser[T] =
    code => Option(parsers.flatMap(parser => parser(code)).head)

  def applyExpr(
      parser: Parser[List[Node]]
  )(f: List[Node] => Node): Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- parser(code)
    } yield PResult(f(tokens), rest)
}
