package parser

import parser.Parser.intNum

object ParserGenerater {
  import Token._

  def skipSpace[T](parser: Parser[T]): Parser[T] =
    code => parser(code.trim)

  def repeat[T](parser: Parser[T]): Parser[List[T]] = code =>
    for {
      PResult(token, rest) <- parser(code)
      PResult(tokens, ret) <- repeat(parser)(rest)
        .orElse(Option(PResult(List[T](), rest)))
    } yield PResult(token +: tokens, ret)

  def repeat[T](allowZero: true)(parser: Parser[T]): Parser[List[T]] =
    code => repeat(parser)(code).orElse(Option(PResult(List[T](), code)))

  def and[A, B](parsers: Or[Parser, A, B]*): Parser[List[A | B]] =
    code => {
      val initial = Option(PResult(List[A | B](), code))
      (initial /: parsers) { (acc, parser) =>
        for {
          PResult(tokens, code) <- acc
          case PResult[(A | B)](token, rest) <- parser(code)
        } yield PResult(tokens :+ token, rest)
      }
    }
  // def and(parsers: Or[Parser, Token, TwoHand]*): Parser[List[(Token | TwoHand)]] =
  //   code => {
  //     val initial = Option(PResult(List[Token | TwoHand](), code))
  //     (initial /: parsers) { (acc, parser) =>
  //       for {
  //         PResult(tokens, code) <- acc
  //         case PResult[(Token | TwoHand)](token, rest) <- parser(code)
  //       } yield PResult(tokens :+ token, rest)
  //     }
  //   }

  def a() = intNum

  // def or(parsers: Parser[Node]*): Parser[Node] = code =>
  //   parsers.flatMap(parser => parser(code)).headOption

  // def makeAst(tokens: List[Node]): Node = {
  //   (tokens.head /: tokens.tail) { (ast, token) =>
  //     (ast, token) match {
  //       case (rhs: Token, op: TwoHand) => op(rhs)
  //       case (op: OneHand, lhs: Token) => op(lhs)
  //       case _                         => ast
  //     }
  //   }
  // }

  // def applyExpr(
  //     parser: Parser[List[Node]]
  // )(f: List[Node] => Node): Parser[Node] = code =>
  //   for {
  //     PResult(tokens, rest) <- parser(code)
  //   } yield PResult(f(tokens), rest)
}
