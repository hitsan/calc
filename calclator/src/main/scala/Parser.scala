object Parser {
  import Ast.Node
  import Ast.Node._
  import Stream._

  case class PResult[+T](
      token: T,
      rest: String
  )
  type Parser[T] = String => Option[PResult[T]]
  type Token = Node | Char | Int | Oprater
  type Oprater = Add.type

  // Parser Generator
  // Skip space
  def skipSpace(parser: Parser[Token]): Parser[Token] =
    code => parser(code.trim)

  def repeat(parser: Parser[Token]): Parser[List[Token]] = code => {
    var isRep = true
    var rest = code
    var tokens: List[Token] = Nil
    while (isRep) {
      parser(rest) match {
        case Some(PResult(token, ret)) => {
          rest = ret
          tokens = tokens.appended(token)
        }
        case None => isRep = false
      }
    }
    if (tokens.nonEmpty) Some(PResult(tokens, rest)) else None
  }

  def chain(parsers: Parser[Token]*): Parser[List[Token]] = code => {
    val initial: Option[PResult[List[Token]]] = Some(PResult(Nil, code))
    parsers.foldLeft(initial) { (acc, parser) =>
      for {
        PResult(tokens, rest) <- acc
        PResult(token, ret) <- parser(rest)
      } yield PResult(tokens.appended(token), ret)
    }
  }
  extension (code: String)
    def isHeadChar(c: Char): Boolean = code.headOption.exists(_ == c)
  extension (code: String)
    def isHeadDigit: Boolean = code.headOption.exists(_.isDigit)

  def parseChar(character: Char): Parser[Char] = code =>
    if (code.headOption.exists(_ == character))
      Some(PResult(character, code.tail))
    else None

  def parseString(str: String): Parser[String] = code =>
    if (code.startsWith(str)) Some(PResult(str, code.drop(str.length)))
    else None

  // Parser
  def parseDigit: Parser[Char] = code =>
    if (code.isHeadDigit) Some(PResult(code.head, code.tail)) else None

  def parseInt: Parser[Int] = code =>
    for {
      PResult(tokens, rest) <- repeat(parseDigit)(code)
    } yield PResult(tokens.mkString.toInt, rest)

  def parsePlus: Parser[Add.type] = code =>
    for {
      PResult(_, rest) <- parseChar('+')(code)
    } yield PResult(Add, rest)

  // orElse
  def choice(parsers: Parser[Token]*): Parser[Token] =
    code => Some(parsers.flatMap(parser => parser(code)).head)

  // Parser
  def term: Parser[Token] = code => ???
  def factor: Parser[Token] = code => ???
  def unary: Parser[Token] = code => primary(code)
  def primary: Parser[Token] = code => parseInt(code)
}
