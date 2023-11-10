package parser

object Primitive {
  import Node._
  import PResult._
  import Combinator._

  def anyChar: Parser[Node] = code =>
    for {
      head <- code.headOption if head.isLetter
    } yield PResult(Achar(head), code.tail)

  def digit: Parser[Node] = code =>
    for {
      head <- code.headOption if head.isDigit
    } yield PResult(IntNum(head.asDigit), code.tail)

  def anyString: Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- rep(anyChar)(code)
    } yield {
      val str = (Str("") /: tokens) { (str, char) =>
        (str, char) match
          case (Str(str), Achar(char)) => Str(str + char)
      }
      PResult(str, rest)
    }

  def intNum: Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- rep(digit)(code)
    } yield {
      val number = (IntNum(0) /: tokens) { (acc, num) =>
        (acc, num) match
          case (IntNum(n1), IntNum(n2)) => IntNum(10 * n1 + n2)
      }
      PResult(number, rest)
    }

  def char(character: Char): Parser[Node] = code =>
    for {
      head <- code.headOption if (head == character)
    } yield PResult(Achar(head), code.tail)

  def string(word: String): Parser[Node] = code =>
    if (code.startsWith(word)) Some(PResult(Str(word), code.drop(word.length)))
    else None

  def charToOp(node: Node): TwoHand = node match {
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
