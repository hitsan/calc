package parser

object Parser {
  import Ast.Node
  import Ast.Node._
  import ParserGenerater._
  import PResult.*

  def anyChar: Parser[Node] = code =>
    for {
      head <- code.headOption if head.isLetter
    } yield PResult(Str(head.toString), code.tail)

  def digit: Parser[Node] = code =>
    for {
      head <- code.headOption if head.isDigit
    } yield PResult(IntNum(head.asDigit), code.tail)

  def anyString: Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- repeat(anyChar)(code)
    } yield {
      val str = tokens.foldLeft(Str("")) { (acc, token) =>
        (acc, token) match {
          case (Str(str1), Str(str2)) => Str(str1 + str2)
          case _                      => Str("") // No reachable
        }
      }
      PResult(str, rest)
    }

  def intNum: Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- repeat(digit)(code)
    } yield {
      val number = tokens.foldLeft(IntNum(0)) { (acc, token) =>
        (acc, token) match {
          case (IntNum(n1), IntNum(n2)) => IntNum(10 * n1 + n2)
          case _                        => IntNum(0) // No reachable
        }
      }
      PResult(number, rest)
    }
  def char(character: Char): Parser[Node] = code =>
    for {
      head <- code.headOption if (head == character)
      str = head.toString
    } yield PResult(Str(str), code.tail)

  def string(word: String): Parser[Node] = code =>
    if (code.startsWith(word)) Some(PResult(Str(word), code.drop(word.length)))
    else None

  def binExpr(op: Node => Node => Node) = op
  val add = binExpr(Add)
  val sub = binExpr(Sub)
  val mul = binExpr(Mul)
  val div = binExpr(Div)
  def operater(op: Char): Parser[Node => Node => Node] = code => {
    val opMap =
      Map((Str("+"), add), (Str("-"), sub), (Str("*"), mul), (Str("/"), div))
    val parser = char(op)
    for {
      PResult(op, rest) <- parser(code)
      oprater <- opMap get op
    } yield PResult(oprater, rest)
  }
}
