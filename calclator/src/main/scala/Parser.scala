object Parser {
  import Ast.Expression
  import Ast.Expression._

  // ParsedResult
  case class PResult[+T](
      expression: T,
      rest: String
  )
  type Parser[T] = PResult[T] => Option[PResult[T]]

  // Parser generator
  def lexeme(parser: Parser[Expression]): Parser[Expression] =
    code => parser(code.trim)

  // def repreat

  // def chainParser(parsers: Parser[Expression]*): Parser[Expression] = { code =>
  //   {
  //     var data: String = code
  //     val a = parsers.foldLeft(List[Token]()) { (tokens, parser) =>
  //       val (token, rest) = parser(data) match {
  //         case Some(token, rest) => (token, rest)
  //       }
  //       data = rest
  //       tokens.appended(token)
  //     }
  //     PResult(tokens.head, data)
  //   }
  // }

  def parseCharacter(char: Char): Parser[Expression] =
    code =>
      if (code(0) == char) Some(PResult(Operator(char), code.drop(1)))
      else None
  def parsePlus = parseCharacter('+')
  def parseMinus = parseCharacter('-')
  def parseTimes = parseCharacter('*')
  def parseDiv = parseCharacter('/')

  def parseString(str: String): Parser[Expression] =
    code =>
      if (code.startsWith(str))
        Some(PResult(Id(str), code.drop(str.length)))
      else None

  // orElse
  def choice(parsers: Parser[Expression]*): Parser[Expression] =
    code => Some(parsers.flatMap(parser => parser(code)).head)

  def parseNum: Parser[Expression] = lexeme(parseInt)

  def parseP(
      parser1: Parser[Expression],
      parser2: Parser[Expression]
  ): Parser[Expression] =
    parser1 andThen (r =>
      r.flatMap(n =>
        n match {
          case PResult(n, r) => parser2(r)
        }
      )
    )

  // Parser
  def parseInt(code: String): Option[PResult[Expression]] = {
    val (num, rest) = code.span(_.isDigit)
    if (num.nonEmpty) Some(PResult(Integer(num.toInt), rest)) else None
  }

  def parseNums(code: String): Option[PResult[Expression]] = {
    parseNum(code) match {
      case Some(c, r) =>
        if (r.nonEmpty) parseNum(r) else Some(PResult(c, r))
      case _ => None
    }
  }

  // def term: Parser[Expression] = code => {
  //   factor(code) match {
  //     case PResult[Expression](e, rest) => (e, rest)
  //   }
  // }

  def factor: Parser[Expression] = code => unary(code)

  def unary: Parser[Expression] = code => primary(code)

  def primary: Parser[Expression] = code => value(code)

  def value: Parser[Expression] = code => parseInt(code)
}
