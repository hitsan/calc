package parser

object Parser {
  import Ast.Node
  import Ast.Node._
  import ParserGenerater._
  import PResult.*

  def anyChar: Parser[NodeT] = code =>
    for {
      head <- code.headOption if head.isLetter
    } yield PResult(CharX(head), code.tail)

  def digit: Parser[NodeT] = code =>
    for {
      head <- code.headOption if head.isDigit
    } yield PResult(IntNum(head.asDigit), code.tail)

  def joinChar(str: NodeT, char: NodeT): Str = (str, char) match {
    case (Str(str), CharX(char)) => Str(str + char)
    case _                       => Str("")
  }
  def anyString: Parser[NodeT] = code =>
    for {
      PResult(tokens, rest) <- repeat(anyChar)(code)
    } yield {
      val str = (Str("") /: tokens) { joinChar(_, _) }
      PResult(str, rest)
    }

  def joinNum(num1: NodeT, num2: NodeT): IntNum = (num1, num2) match {
    case (IntNum(n1), IntNum(n2)) => IntNum(10 * n1 + n2)
    case _                        => IntNum(0)
  }
  def intNum: Parser[NodeT] = code =>
    for {
      PResult(tokens, rest) <- repeat(digit)(code)
    } yield {
      val number = (IntNum(0) /: tokens) { joinNum(_, _) }
      PResult(number, rest)
    }

  def char(character: Char): Parser[NodeT] = code =>
    for {
      head <- code.headOption if (head == character)
    } yield PResult(CharX(head), code.tail)

  def string(word: String): Parser[NodeT] = code =>
    if (code.startsWith(word)) Some(PResult(Str(word), code.drop(word.length)))
    else None

  def operater(op: Char): Parser[NodeT] = code => {
    for {
      PResult(token, rest) <- char(op)(code)
    } yield {
      token match {
        case CharX('+') => PResult(add, rest)
        case CharX('-') => PResult(sub, rest)
        case CharX('*') => PResult(mul, rest)
        case CharX('/') => PResult(div, rest)
      }
    }
  }
}
