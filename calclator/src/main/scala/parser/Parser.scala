package parser

object Parser {
  import Token._
  import ParserGenerater._
  import PResult.*

  def anyChar: Parser[Node] = code =>
    for {
      head <- code.headOption if head.isLetter
    } yield PResult(Achar(head), code.tail)

  def digit: Parser[Node] = code =>
    for {
      head <- code.headOption if head.isDigit
    } yield PResult(IntNum(head.asDigit), code.tail)

  def joinChar(str: Node, char: Node): Str = (str, char) match {
    case (Str(str), Achar(char)) => Str(str + char)
    case _                       => Str("")
  }
  def anyString: Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- repeat(anyChar)(code)
    } yield {
      val str = (Str("") /: tokens) { joinChar(_, _) }
      PResult(str, rest)
    }

  def joinNum(num1: Node, num2: Node): IntNum = (num1, num2) match {
    case (IntNum(n1), IntNum(n2)) => IntNum(10 * n1 + n2)
    case _                        => IntNum(0)
  }
  def intNum: Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- repeat(digit)(code)
    } yield {
      val number = (IntNum(0) /: tokens) { joinNum(_, _) }
      PResult(number, rest)
    }

  def char(character: Char): Parser[Node] = code =>
    for {
      head <- code.headOption if (head == character)
    } yield PResult(Achar(head), code.tail)

  def string(word: String): Parser[Node] = code =>
    if (code.startsWith(word)) Some(PResult(Str(word), code.drop(word.length)))
    else None

  // def operater(op: OpChar): Parser[Node] = code => char(op)(code)
  def operater(op: Operater): Parser[TwoHand] = code =>
    for {
      PResult(token, rest) <- char(op)(code)
    } yield {
      token match {
        case Achar('+') => PResult(add, rest)
        case Achar('-') => PResult(sub, rest)
        case Achar('*') => PResult(mul, rest)
        case Achar('/') => PResult(div, rest)
        case _          => null
      }
    }
}
