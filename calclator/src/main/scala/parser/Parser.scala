package parser

object Parser {
  import Ast.Node
  import Ast.Node._
  import ParserGenerater._
  import PResult.*

  def anyChar: Parser[Node] = code =>
    for {
      head <- code.headOption if head.isLetter
    } yield PResult(CharX(head), code.tail)

  def digit: Parser[Node] = code =>
    for {
      head <- code.headOption if head.isDigit
    } yield PResult(IntNum(head.asDigit), code.tail)

  def anyString: Parser[Node] = code => {
    def joinChar(str: Node, char: Node): Str = (str, char) match {
      case (Str(str), CharX(char)) => Str(str + char)
      case _                       => Str("")
    }
    for {
      PResult(tokens, rest) <- repeat(anyChar)(code)
    } yield {
      val str = (Str("") /: tokens) { joinChar(_, _) }
      PResult(str, rest)
    }
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
    } yield PResult(CharX(head), code.tail)

  def string(word: String): Parser[Node] = code =>
    if (code.startsWith(word)) Some(PResult(Str(word), code.drop(word.length)))
    else None

  // This function can't directry return Operaters(like Add, Sub, Mul, Div).
  // Want to directry return Operaters.
  def operater(op: Char): Parser[Node] = code => char(op)(code)
  // def m[A, B](op: Char): Parser[A <: B] = code => char(op)(code)
}
