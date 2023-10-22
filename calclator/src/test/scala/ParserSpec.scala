class ParserSpec extends munit.FunSuite {
  import Parser._
  import Ast.Node._
  import Ast.Node

  // Initial
  test("isDigit") {
    assertEquals(parseDigit("1+1"), Some(PResult('1', "+1")))
    assertEquals(parseDigit(""), None)
    assertEquals(parseDigit("a"), None)
  }

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

  test("number") {
    assertEquals(parseInt("123"), Some(PResult(Integer(123), "")))
    assertEquals(parseInt("12+3"), Some(PResult(Integer(12), "+3")))
    assertEquals(parseInt("+123"), None)
    assertEquals(parseInt(""), None)
  }

  test("Op Plus") {
    val parsePlus = parseOpe('+')
    assertEquals(parsePlus("+"), Some(PResult(Add, "")))
    assertEquals(parsePlus("++"), Some(PResult(Add, "+")))
    assertEquals(parsePlus("+1"), Some(PResult(Add, "1")))
    assertEquals(parsePlus("1+1"), None)
    assertEquals(parsePlus(""), None)
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

  test("primary") {
    assertEquals(primary("123"), Some(PResult(Integer(123), "")))
    assertEquals(primary("12+3"), Some(PResult(Integer(12), "+3")))
    assertEquals(primary("123"), Some(PResult(Integer(123), "")))
    assertEquals(primary("123abc"), Some(PResult(Integer(123), "abc")))
    assertEquals(primary("abc"), Some(PResult("abc", "")))
    assertEquals(primary("abc123"), Some(PResult("abc", "123")))
    assertEquals(primary("+123"), None)
    assertEquals(primary(""), None)
  }

  test("factor") {
    assertEquals(factor("1*2"), Some(PResult(Mul(Integer(1))(Integer(2)), "")))
    assertEquals(factor("1*2*3"), Some(PResult(Mul(Mul(Integer(1))(Integer(2)))(Integer(3)), "")))
    assertEquals(factor(""), None)
    assertEquals(factor("1+2"), None)
    assertEquals(factor("*1*2"), None)
  }
}
