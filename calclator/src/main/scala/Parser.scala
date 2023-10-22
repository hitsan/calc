import Ast.Node
import Ast.Node._

object Parser {
  case class PResult[+T](
      token: T,
      rest: String
  )
  type Parser[T] = String => Option[PResult[T]]
  type Token = Node | Char | Int | Oprater | String
  type Oprater = Add.type | Sub.type | Mul.type | Div.type

  // Parser Generator
  // Skip space
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

  // Parser
  def parserAnyChar: Parser[Char] = code => code.parseMatchChar(_.isLetter)
  def parseDigit: Parser[Char] = code => code.parseMatchChar(_.isDigit)
  def parseAnyString: Parser[String] = code =>
    for {
      PResult(tokens, rest) <- repeat(parserAnyChar)(code)
    } yield PResult(tokens.mkString, rest)

  def parseInt: Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- repeat(parseDigit)(code)
    } yield {
      val number = tokens.mkString.toInt
      PResult(Integer(number), rest)
    }

  def parseOpe(op: Char): Parser[Oprater] = code => {
    val opMap = Map(('+', Add), ('-', Sub), ('*', Mul), ('/', Div))
    for {
      PResult(op, rest) <- parseChar('+')(code)
      oprater <- opMap get op
    } yield PResult(oprater, rest)
  }

  // Parser
  def term: Parser[Token] = code => ???
  def factor: Parser[Token] = code => ???

  def unary: Parser[Token] = code => primary(code)
  def primary: Parser[Token] = code =>
    parseInt(code).orElse(parseAnyString(code))
}
