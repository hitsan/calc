package parser

object Combinator {
  import scala.collection.mutable.ListBuffer
  import Node._

  def repBase[A](parser: Parser[A], allowEmpty: Boolean): Parser[List[A]] = {
    def loop(
        code: String,
        acc: ListBuffer[A] = ListBuffer()
    ): Option[PResult[List[A]]] = {
      parser(code) match {
        case Some(PResult(token, rest))         => loop(rest, acc += token)
        case None if acc.isEmpty && !allowEmpty => None
        case None => Some(PResult(acc.toList, code))
      }
    }
    loop(_)
  }

  def rep[A](parser: Parser[A]): Parser[List[A]] =
    repBase(parser, allowEmpty = false)
  def rep0[A](parser: Parser[A]): Parser[List[A]] =
    repBase(parser, allowEmpty = true)

  extension [A](parser: Parser[A]) def * = rep0(parser)

  def and[A, B](parsers: OrParser[A, B]*): Parser[List[A | B]] = code => {
    val initial = Option(PResult(List[A | B](), code))
    parsers.foldLeft(initial) { (acc, parser) =>
      for {
        PResult(tokens, code) <- acc
        case PResult[(A | B)](token, rest) <- parser(code)
      } yield PResult(tokens :+ token, rest)
    }
  }

  extension [A, B](parser: OrParser[A, B])
    def &(parser2: OrParser[A, B]) = and(parser, parser2)

  def or[A](parsers: Parser[A]*): Parser[A] = code =>
    parsers.flatMap(parser => parser(code)).headOption

  extension [A](parser: Parser[A])
    def |(parser2: Parser[A]) = or(parser, parser2)

  def astRule[A <: List[_]](tokens: A): Node = {
    val initial = tokens.head match {
      case head: List[_] => astRule(head)
      case head: Ast     => head
    }
    tokens.tail.foldLeft(initial) { (ast, token) =>
      (ast, token) match {
        case (n: Node, l: List[_])    => astRule(n +: l)
        case (rhs: Node, op: TwoHand) => op(rhs)
        case (op: OneHand, lhs: Node) => op(lhs)
        case (_, _)                   => ast
      }
    } match {
      case n: Node => n
      case _       => sys.error("Invalid token")
    }
  }

  def uneryRule[A <: List[_]](tokens: A): Node =
    (tokens.head, tokens.last) match {
      case (op: OneHand, rhs: Node)    => op(rhs)
      case (_, _)                     => sys.error("Invalid token")
    }

  def exprRule[A <: List[_]](tokens: A): Node =
    (tokens.head, tokens(1), tokens.last) match {
      case (Achar('('), n: Node, Achar(')')) => n
    }

  extension [A](parser: Parser[A])
    def struct(f: A => Node): Parser[Node] = code =>
      parser(code).map { case PResult(tokens, rest) =>
        PResult(f(tokens), rest)
      }
}
