package parser

object Primitive {
  import Node._
  import PResult._
  import Combinator._

  // Don't allow space
  def anyCharS: Parser[Node] = code =>
    for {
      head <- code.headOption if head.isLetter
    } yield PResult(Achar(head), code.tail)

  def digitS: Parser[Node] = code =>
    for {
      head <- code.headOption if head.isDigit
    } yield PResult(IntNum(head.asDigit), code.tail)

  def anyStringS: Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- rep(anyCharS)(code)
    } yield {
      val str = (Str("") /: tokens) { (str, char) =>
        (str, char) match
          case (Str(str), Achar(char)) => Str(str + char)
      }
      PResult(str, rest)
    }

  def intNumS: Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- rep(digitS)(code)
    } yield {
      val number = (IntNum(0) /: tokens) { (acc, num) =>
        (acc, num) match
          case (IntNum(n1), IntNum(n2)) => IntNum(10 * n1 + n2)
      }
      PResult(number, rest)
    }

  def charS(character: Char): Parser[Node] = code =>
    for {
      head <- code.headOption if (head == character)
    } yield PResult(Achar(head), code.tail)

  def stringS(word: String): Parser[Node] = code =>
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
      PResult(token, rest) <- charS(op)(code)
    } yield PResult(charToOp(token), rest)

  def skipSpace[A](parser: Parser[A]): Parser[A] =
    code => parser(code.trim)

  // Blow functions can skip space
  def anyChar = skipSpace(anyCharS)
  def digit = skipSpace(digitS)
  def anyString = skipSpace(anyStringS)
  def intNum = skipSpace(intNumS)
  def char(character: Char) = skipSpace(charS(character))
  def string(word: String) = skipSpace(stringS(word))
  def plus = skipSpace(operater('+'))
  def minus = skipSpace(operater('-'))
  def times = skipSpace(operater('*'))
  def divide = skipSpace(operater('/'))
}
