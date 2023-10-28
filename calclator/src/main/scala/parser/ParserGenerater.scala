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

  def combineTokens(tokens: List[AA]): Node = 
    val toks = tokens.tail.reverse
    toks.foldRight(tokens.head)( (acc, token) =>
      acc match {
        case a: Node=>Node => a(token)
        case a: Node => token(a)
      }
    )

  def chain2(parsers: Parser[AA]*): Parser[Node] = code => {
    var rest = code
    val initial: Option[List[AA]] = Some(List[AA]())
    val tokens = parsers.foldLeft(initial)((acc, parser) =>
      for {
        PResult(token, ret) <- parser(rest)
        a <- acc
        rest = ret
      } yield a :+ token
    )
    tokens.exists(Some(PResult(combineTokens(tokens.get), rest)))
  }

  def combExpr(ope: Operater)(rhs: Node): Node => Node = ope(rhs)

  def comb(prev: Parser[Operater], next: Parser[Node]): Parser[Node => Node] =
    code =>
      for {
        PResult(preToken, preRest) <- prev(code)
        PResult(nextToken, nextRest) <- next(preRest)
      } yield PResult(combExpr(preToken)(nextToken), nextRest)

  def comb2(prev: Parser[Node], next: Parser[Node => Node]): Parser[Node] =
    code =>
      for {
        PResult(preToken, preRest) <- prev(code)
        PResult(nextToken, nextRest) <- next(preRest)
      } yield PResult(nextToken(preToken), nextRest)

  def choice[T](parsers: Parser[T]*): Parser[T] =
    code => Some(parsers.flatMap(parser => parser(code)).head)

  def parseChar(character: Char): Parser[Node] = code =>
    for {
      head <- code.headOption if (head == character)
      str = head.toString
    } yield PResult(Str(str), code.tail)

  def parseString(word: String): Parser[Node] = code =>
    if (code.startsWith(word)) Some(PResult(Str(word), code.drop(word.length)))
    else None
}
