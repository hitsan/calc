class ParserGeneraterSpec extends munit.FunSuite {
  import parser._
  import parser.Parser._
  import parser.ParserGenerater._
  import parser.PResult
  import parser.Ast.Node._
  import parser.Ast.Node

  test("repeat") {
    val number = repeat(parseDigit)
    assertEquals(number("123"), Some(PResult(List(IntNum(1),IntNum(2),IntNum(3)), "")))
    assertEquals(number("123+12"), Some(PResult(List(IntNum(1),IntNum(2),IntNum(3)), "+12")))
    assertEquals(number(""), None)
  }

  test("repeat Zero or More") {
    val number = repeat(true)(parseDigit)
    assertEquals(number("123"), Some(PResult(List(IntNum(1),IntNum(2),IntNum(3)), "")))
    assertEquals(number("123+12"), Some(PResult(List(IntNum(1),IntNum(2),IntNum(3)), "+12")))
    assertEquals(number(""), Some(PResult(Nil, "")))
  }

  // test("chain") {
  //   val parser = chain(parseInt, parseOp('+'), parseInt)
  //   assertEquals(parser("1+2"), Some(PResult(List(IntNum(1), Add, IntNum(2)), "")))
  //   assertEquals(parser("1+2+3"), Some(PResult(List(IntNum(1), Add, IntNum(2)), "+3")))
  //   assertEquals(parser("11+2"), Some(PResult(List(IntNum(11), Add, IntNum(2)), "")))
  //   assertEquals(parser("a+2"), None)
  //   assertEquals(parser("11-2"), None)
  //   assertEquals(parser(""), None)
  // }

  test("comb with function") {
    val parser = comb(parseOp('+'), parseInt)
    assertEquals(parser("a+2"), None)
    assertEquals(parser("11-2"), None)
    assertEquals(parser(""), None)

    val parser1 = comb2(parseInt, comb(parseOp('+'), parseInt))
    assertEquals(parser1("1+2"), Some(PResult(Add(IntNum(2))(IntNum(1)), "")))
    assertEquals(parser1("1+2+3"), Some(PResult(Add(IntNum(2))(IntNum(1)), "+3")))
  }

  test("String test") {
    val parser = parseString("if")
    assertEquals(parser("if"), Some(PResult(Str("if"), "")))
    assertEquals(parser("ifelse"), Some(PResult(Str("if"), "else")))
    assertEquals(parser("11+2"), None)
    assertEquals(parser(""), None)
  }


}