package parser

object Eval {
  import Token._
  import Parser._
  import ParserGenerater._

  // Parser Expression
  // def term: Parser[Node] = code => ???

  // def factor[A]: Parser[List[Token | List[List[TwoHand | Token]]]] =
  def factor[A]: Parser[List[Token | List[List[TwoHand | Token]]]] =
    and(unary, repeat0(and(operater('*'), unary)))

  def unary: Parser[Token] = primary
  def primary: Parser[Token] = or(intNum, anyString)
}
