class ParserSpec extends munit.FunSuite {
  import parser.Parser._
  import parser.ParserGenerater._
  import parser.PResult
  import parser.Ast.Node._
  import parser.Ast.Node

  // Initial
  test("isDigit") {
    assertEquals(parseDigit("1+1"), Some(PResult(IntNum(1), "+1")))
    assertEquals(parseDigit(""), None)
    assertEquals(parseDigit("a"), None)
  }

  test("number") {
    assertEquals(parseInt("12+3"), Some(PResult(IntNum(12), "+3")))
    assertEquals(parseInt("+123"), None)
    assertEquals(parseInt(""), None)
  }

  test("Op Plus") {
    val parsePlus = parseOp('+')
    assertEquals(parsePlus("+"), Some(PResult(add, "")))
    assertEquals(parsePlus("++"), Some(PResult(add, "+")))
    assertEquals(parsePlus("+1"), Some(PResult(add, "1")))
    assertEquals(parsePlus("1+1"), None)
    assertEquals(parsePlus(""), None)

    assertEquals(add(IntNum(1))(IntNum(1)), Add(IntNum(1))(IntNum(1)))
  }

  test("primary") {
    assertEquals(primary("123"), Some(PResult(IntNum(123), "")))
    assertEquals(primary("12+3"), Some(PResult(IntNum(12), "+3")))
    assertEquals(primary("123"), Some(PResult(IntNum(123), "")))
    assertEquals(primary("123abc"), Some(PResult(IntNum(123), "abc")))
    assertEquals(primary("abc"), Some(PResult(Str("abc"), "")))
    assertEquals(primary("abc123"), Some(PResult(Str("abc"), "123")))
    assertEquals(primary("+123"), None)
    assertEquals(primary(""), None)
  }

  // test("factor") {
  //   assertEquals(factor("1*2"), Some(PResult(Mul(Integer(1))(Integer(2)), "")))
  //   assertEquals(factor("1*2*3"), Some(PResult(Mul(Mul(Integer(1))(Integer(2)))(Integer(3)), "")))
  //   assertEquals(factor(""), None)
  //   assertEquals(factor("1+2"), None)
  //   assertEquals(factor("*1*2"), None)
  // }
}