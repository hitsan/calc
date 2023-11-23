import parser.PResult
class ParserSpec extends munit.FunSuite {
  import parser.Parser
  import parser.Parser.ParseError
  import parser.Node._

  test("parser should return Right(tokens) when the expression is valid") {
    val code = "1 + 2 * 3"
    val expectedTokens = Add(IntNum(1), Mul(IntNum(2), IntNum(3)))
    val result = Parser.parser(code)
    assertEquals(result, Right(expectedTokens))
  }

  test("parser should return Left(ParseError) when there is a parse error") {
    val code = "1 +"
    val expectedError = ParseError("Invalid syntax: +")
    val result = Parser.parser(code)
    assertEquals(result, Left(expectedError))
  }

  test("parser should return Left(ParseError) when the syntax is invalid") {
    val code = "1 + 2 *"
    val expectedError = ParseError(s"Invalid syntax: $code")
    val result = Parser.parser(code)
    assertEquals(result, Left(expectedError))
  }
}
