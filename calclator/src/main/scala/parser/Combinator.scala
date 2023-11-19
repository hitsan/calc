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

}
