package parser

import Ast.Node
import Ast.Node._
import ParserGenerater._
import PResult.*

object Parser {
  def parserAnyChar: Parser[Node] = code =>
    for {
      head <- code.headOption if head.isLetter
    } yield PResult(Str(head.toString), code.tail)

  def parseDigit: Parser[Node] = code =>
    for {
      head <- code.headOption if head.isDigit
      num = head.asDigit
    } yield PResult(IntNum(num), code.tail)

  def parseAnyString: Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- repeat(parserAnyChar)(code)
    } yield {
      val str = tokens.foldLeft(Str("")) { (acc, token) =>
        (acc, token) match {
          case (Str(str1), Str(str2)) => Str(str1 + str2)
          case _                      => Str("") // No reachable
        }
      }
      PResult(str, rest)
    }

  def parseInt: Parser[Node] = code =>
    for {
      PResult(tokens, rest) <- repeat(parseDigit)(code)
    } yield {
      val number = tokens.foldLeft(IntNum(0)) { (acc, token) =>
        (acc, token) match {
          case (IntNum(n1), IntNum(n2)) => IntNum(10 * n1 + n2)
          case _                        => IntNum(0) // No reachable
        }
      }
      PResult(number, rest)
    }

  def parseOp(op: Char): Parser[Oprater] = code => {
    val opMap =
      Map((Str("+"), Add), (Str("-"), Sub), (Str("*"), Mul), (("/"), Div))
    for {
      PResult(op, rest) <- parseChar('+')(code)
      oprater <- opMap get op
    } yield PResult(oprater, rest)
  }

  // Parser Expression
  def term: Parser[Node] = code => ???
  def factor: Parser[Node] = code => ???

  def unary: Parser[Node] = code => primary(code)
  def primary: Parser[Node] = code =>
    parseInt(code).orElse(parseAnyString(code))
}
