package parser

object Parser {
  import Expression.expression

  case class ParseError(message: String)

  def parser(code: String): Either[ParseError, Node] =
    expression(code) match {
      case Some(PResult(tokens, "")) => Right(tokens)
      case Some(PResult(tokens, rest)) =>
        Left(ParseError(s"Invalid syntax: $rest"))
      case None => Left(ParseError(s"Invalid syntax: $code"))
    }
}
