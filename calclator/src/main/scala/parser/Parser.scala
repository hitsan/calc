package parser

object Parser {
  import Token._
  import PResult._
  import ParserGenerater._

  def anyChar: Parser[Token] = code =>
    for {
      head <- code.headOption if head.isLetter
    } yield PResult(Achar(head), code.tail)

  def digit: Parser[Token] = code =>
    for {
      head <- code.headOption if head.isDigit
    } yield PResult(IntNum(head.asDigit), code.tail)

  def joinChar(str: Token, char: Token): Str = (str, char) match {
    case (Str(str), Achar(char)) => Str(str + char)
    case _                       => Str("")
  }
  def anyString: Parser[Token] = code =>
    for {
      PResult(tokens, rest) <- rep(anyChar)(code)
    } yield {
      val str = (Str("") /: tokens) { joinChar(_, _) }
      PResult(str, rest)
    }

  def joinNum(num1: Token, num2: Token): Token = (num1, num2) match {
    case (IntNum(n1), IntNum(n2)) => IntNum(10 * n1 + n2)
    case _                        => IntNum(0)
  }
  def intNum: Parser[Token] = code =>
    for {
      PResult(tokens, rest) <- rep(digit)(code)
    } yield {
      val number = (IntNum(0) /: tokens) { joinNum(_, _) }
      PResult(number, rest)
    }

  def char(character: Char): Parser[Token] = code =>
    for {
      head <- code.headOption if (head == character)
    } yield PResult(Achar(head), code.tail)

  def string(word: String): Parser[Token] = code =>
    if (code.startsWith(word)) Some(PResult(Str(word), code.drop(word.length)))
    else None

  def charToOp(node: Token): TwoHand = node match {
    case Achar('+') => add
    case Achar('-') => sub
    case Achar('*') => mul
    case Achar('/') => div
    case _          => null
  }
  def operater(op: Operater): Parser[TwoHand] = code =>
    for {
      PResult(token, rest) <- char(op)(code)
    } yield PResult(charToOp(token), rest)
}
