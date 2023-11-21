package parser

object Parser {
  def parse(code: String): Either[ParserError, Node] = {
    import Expression._
    expression(code) match {
        case None => Left(getParseError(code))
        case Some(tokens, rest) => if(rest == "") Right(tokens) else Left(getParseError(rest))
    }
  }

  def getParseError(code: String): ParserError = ParserError(code)
}