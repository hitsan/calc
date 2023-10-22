class ParserGeneraterSpec extends munit.FunSuite {
  import parser.Parser._
  import parser.ParserGenerater._
  import parser.PResult
  import parser.Ast.Node._
  import parser.Ast.Node

  test("repeat") {
    val number = repeat(parseDigit)
    assertEquals(number("123"), Some(PResult(List('1','2','3'), "")))
    assertEquals(number("123+12"), Some(PResult(List('1','2','3'), "+12")))
    assertEquals(number(""), None)
  }

  test("repeat Zero or More") {
    val number = repeatZeroOrMore(parseDigit)
    assertEquals(number("123"), Some(PResult(List('1','2','3'), "")))
    assertEquals(number("123+12"), Some(PResult(List('1','2','3'), "+12")))
    assertEquals(number(""), Some(PResult(Nil, "")))
  }

  test("chain") {
    val parser = chain(parseInt, parseOpe('+'), parseInt)
    assertEquals(parser("1+2"), Some(PResult(List(Integer(1), Add, Integer(2)), "")))
    assertEquals(parser("1+2+3"), Some(PResult(List(Integer(1), Add, Integer(2)), "+3")))
    assertEquals(parser("11+2"), Some(PResult(List(Integer(11), Add, Integer(2)), "")))
    assertEquals(parser("a+2"), None)
    assertEquals(parser("11-2"), None)
    assertEquals(parser(""), None)
  }

  test("String test") {
    val parser = parseString("if")
    assertEquals(parser("if"), Some(PResult("if", "")))
    assertEquals(parser("ifelse"), Some(PResult("if", "else")))
    assertEquals(parser("11+2"), None)
    assertEquals(parser(""), None)
  }


}