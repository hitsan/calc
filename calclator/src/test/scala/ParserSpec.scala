class ParserSpec extends munit.FunSuite {
  import parser.Parser._
  import parser.ParserError
  import parser.Node._
  test("parser with invalid characters") {
    assertEquals(parse("1a"), Left(ParserError("a")))
    assertEquals(parse("1+*2"), Left(ParserError("+*2")))
    assertEquals(parse("1+1++2"), Left(ParserError("++2")))
  }

  test("expression with missing operands") {
    assertEquals(parse("1+"), Left(ParserError("+")))
    assertEquals(parse("*2"), Left(ParserError("*2")))
    assertEquals(parse("/"), Left(ParserError("/")))
  }

  test("expression with unbalanced parentheses") {
    assertEquals(parse("(1+2"), Left(ParserError("(1+2")))
    assertEquals(parse("1+2)"), Left(ParserError(")")))
    assertEquals(parse("((1+2)"), Left(ParserError("((1+2)")))
    assertEquals(parse("(1+2))"), Left(ParserError(")")))
  }

  test("comparison operators - invalid expressions") {
    assertEquals(parse("1 <"), Left(ParserError(" <")))
    assertEquals(parse("> 2"), Left(ParserError("> 2")))
    assertEquals(parse("1 <= 2 >"), Left(ParserError(" >")))
    assertEquals(
      parse("1 >= 2 =="),
      Left(ParserError(" =="))
    )
    assertEquals(
      parse("1 !="),
      Left(ParserError(" !="))
    )
  }

  test("parser") {
    assertEquals(parse("1+1"), Right(Add(IntNum(1), IntNum(1))))
  }
}
