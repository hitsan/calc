package parser

import Ast.Node
import Ast.Node._
import ParserGenerater._
import PResult.* 

object Parser {
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

  // Parser Expression
  def term: Parser[Token] = code => ???
  def factor: Parser[Token] = code => ???

  def unary: Parser[Token] = code => primary(code)
  def primary: Parser[Token] = code =>
    parseInt(code).orElse(parseAnyString(code))
}
