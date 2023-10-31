package parser

object ParserGenerater {
  import Ast.Node
  import Ast.Node._
  def skipSpace[T](parser: Parser[T]): Parser[T] =
    code => parser(code.trim)

  def repeat[T](parser: Parser[T]): Parser[List[T]] = code =>
    repeat(true)(parser)(code) match {
      case Some(Nil, _) => None
      case value        => value
    }

  def repeat[T](allowZero: true)(parser: Parser[T]): Parser[List[T]] = code => {
    var isRep = true
    var rest = code
    var tokens: List[T] = Nil
    while (isRep) {
      parser(rest) match {
        case Some(PResult(token, ret)) => {
          rest = ret
          tokens = tokens.appended(token)
        }
        case None => isRep = false
      }
    }
    Option(PResult(tokens, rest))
  }

  def chain[T](parsers: Parser[T]*): Parser[List[T]] = code => {
    val initial = Option(PResult(List[T](), code))
    (initial /: parsers) { (acc, parser) =>
      for {
        PResult(tokens, code) <- acc
        PResult(token, rest) <- parser(code)
      } yield PResult(tokens:+token, rest)
    }
  }

  def comb(op: Operater)(node: Node): OneHadNode = op(node)
  def comb1(node: Node)(op: OneHadNode): Node = op(node)
  // def comb2(rhs: OneHadNode)(node2: OneHadNode): OneHadNode = lhs => node2(rhs)(lhs)
  // AND(Binary)
  def and[A, B, C](f: A => B => C)(cur: Parser[A], next: Parser[B]): Parser[C] =
    code =>
      for {
        PResult(curToken, curRest) <- cur(code)
        PResult(nextToken, nextRest) <- next(curRest)
      } yield PResult(f(curToken)(nextToken), nextRest)

  // OR
  def or[T](parsers: Parser[T]*): Parser[T] =
    code => Option(parsers.flatMap(parser => parser(code)).head)

  // def applyExpr(parsers: Parser[A]*)(f: A => A): Parser[A] = {
  // ???
  // }
}
