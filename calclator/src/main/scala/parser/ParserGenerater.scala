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
    Some(PResult(tokens, rest))
  }

  def chain[T](parsers: Parser[T]*): Parser[List[T]] = code => {
    val initial: Option[PResult[List[T]]] = Some(PResult(Nil, code))
    parsers.foldLeft(initial) { (acc, parser) =>
      for {
        PResult(tokens, rest) <- acc
        PResult(token, ret) <- parser(rest)
      } yield PResult(tokens.appended(token), ret)
    }
  }

  def applyExpr(ast: Node, token: Node): Node = {
    // case Mul      => Mul(ast)
    // case tok: Int => ast(tok)
    ???
  }

  def chain[T](parsers: Parser[T]*)(f: Token => Token): Parser[T] = code => {
    val initial: Option[PResult[List[T]]] = Some(PResult(Nil, code))
    parsers.foldLeft(initial) { (acc, parser) =>
      for {
        PResult(tokens, rest) <- acc
        PResult(token, ret) <- parser(rest)
      } yield PResult(tokens.appended(token), ret)
    }
    ???
  }

  def choice[T](parsers: Parser[T]*): Parser[T] =
    code => Some(parsers.flatMap(parser => parser(code)).head)

  extension (code: String)
    def parseMatchChar(f: Char => Boolean): Option[PResult[Char]] = for {
      head <- code.headOption if f(head)
    } yield PResult(head, code.tail)

  def parseChar(character: Char): Parser[Char] = code =>
    code.parseMatchChar(_ == character)

  def parseString(word: String): Parser[Token] = code =>
    if (code.startsWith(word)) Some(PResult(word, code.drop(word.length)))
    else None
}
