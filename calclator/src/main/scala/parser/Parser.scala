package parser

object Parser {
  import Expression.expression

  def parse(code: String): Either[ParseError, Node] =
    expression(code) match {
      case Some(PResult(tokens, "")) => Right(tokens)
      case Some(PResult(tokens, rest)) =>
        Left(ParseError(s"Invalid syntax: $rest"))
      case None => Left(ParseError(s"Invalid syntax: $code"))
    }
}
