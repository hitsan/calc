object Parser {
  import Ast.Node
  import Ast.Node._

  // ParsedResult
  case class PResult[+T](
      node: T,
      rest: String
  )
  type Parser[T] = String => Option[PResult[T]]

  // Parser generator
  def lexeme(parser: Parser[Node]): Parser[Node] =
    code => parser(code.trim)

  def parseCharacter(char: Char): Parser[Node] =
    code =>
      if (code(0) == char) Some(PResult(Operator(char), code.drop(1)))
      else None
  def parsePlus = parseCharacter('+')
  def parseMinus = parseCharacter('-')
  def parseTimes = parseCharacter('*')
  def parseDiv = parseCharacter('/')

  //parseEnd

  def parseString(str: String): Parser[Node] =
    code =>
      if (code.startsWith(str))
        Some(PResult(Id(str), code.drop(str.length)))
      else None

  // orElse
  def choice(parsers: Parser[Node]*): Parser[Node] =
    code => Some(parsers.flatMap(parser => parser(code)).head)

  def parseNum: Parser[Node] = lexeme(parseInt)

  def parseP(
      parser1: Parser[Node],
      parser2: Parser[Node]
  ): Parser[Node] =
    parser1 andThen (r =>
      r.flatMap(n =>
        n match {
          case PResult(n, r) => parser2(r)
        }
      )
    )

  // Parser
  def parseInt(code: String): Option[PResult[Node]] = {
    val (num, rest) = code.span(_.isDigit)
    if (num.nonEmpty) Some(PResult(Integer(num.toInt), rest)) else None
  }

  def parseNums(code: String): Option[PResult[Node]] = {
    parseNum(code) match {
      case Some(c, r) =>
        if (r.nonEmpty) parseNum(r) else Some(PResult(c, r))
      case _ => None
    }
  }
}
