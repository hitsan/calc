object Parser {
  import Ast.Node
  import Ast.Node._

  case class PResult[+T](
      token: T,
      rest: String
  )
  type Parser[T] = String => Option[PResult[T]]
  type Token = Node | Char | Int

  // Parser Generator
  // Skip space
  def skipSpace(parser: Parser[Node]): Parser[Node] =
    code => parser(code.trim)

  def repeat(parser: Parser[Token]): Parser[List[Token]] = code =>
    var tokens: List[Token] = Nil
    val con = parser(code) match {
      case Some(_) => true
      case _ => false
    }
    for {
      PResult(token, rest) <- parser(code)
    } yield 

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
    if (code.head == character) Some(PResult(character, code.tail)) else None

  def parseString(str: String): Parser[String] = code =>
    if (code.startsWith(str)) Some(PResult(str, code.drop(str.length)))
    else None

  // Parser
  def parseDigit: Parser[Char] = code =>
    if (code.isHeadDigit) Some(PResult(code.head, code.tail)) else None

  def parseInt: Parser[Node] = code => {
    val (num, rest) = code.span(_.isDigit)
    if (num.nonEmpty) Some(PResult(Integer(num.toInt), rest)) else None
  }

  def parsePlus: Parser[Node => Node => Node] = code =>
    (code(0), code.tail) match {
      case ('+', rest) => Some(PResult(Add, rest))
      case _           => None
    }

  // orElse
  def choice(parsers: Parser[Node]*): Parser[Node] =
    code => Some(parsers.flatMap(parser => parser(code)).head)

  def parseNum: Parser[Node] = skipSpace(parseInt)

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
  def parseNums(code: String): Option[PResult[Node]] = {
    parseNum(code) match {
      case Some(c, r) =>
        if (r.nonEmpty) parseNum(r) else Some(PResult(c, r))
      case _ => None
    }
  }

  // def term: Parser[Node] = code => {
  //   factor(code) match {
  //     case PResult[Node](e, rest) => (e, rest)
  //   }
  // }

  def factor: Parser[Node] = code => {
    // val p = for {
    //   PResult(c, r) <- unary(code)
    //   _ <- parseDiv(c)
    // } yield parseDiv(c)

    // for {
    //   pp <- p
    //   PResult(c, r) <- unary(code)
    // }
    ???
  }

  def unary: Parser[Node] = code => primary(code)

  def primary: Parser[Node] = code => parseInt(code)
}
