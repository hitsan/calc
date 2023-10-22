package parser

object ParserGenerater {
  def skipSpace[T](parser: Parser[T]): Parser[T] =
    code => parser(code.trim)

  def repeat[T](parser: Parser[T]): Parser[List[T]] = code =>
    repeatZeroOrMore(parser)(code) match {
      case Some(Nil, _) => None
      case value        => value
    }

  def repeatZeroOrMore[T](parser: Parser[T]): Parser[List[T]] = code => {
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