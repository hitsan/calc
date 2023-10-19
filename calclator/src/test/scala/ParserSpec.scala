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

  test("number") {
    assertEquals(parseInt("123"), Some(PResult(123, "")))
    assertEquals(parseInt("12+3"), Some(PResult(12, "+3")))
    assertEquals(parseInt("123"), Some(PResult(123, "")))
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
    assertEquals(parser("1+2"), Some(PResult(List(1, Add, 2), "")))
    assertEquals(parser("1+2+3"), Some(PResult(List(1, Add, 2), "+3")))
    assertEquals(parser("11+2"), Some(PResult(List(11, Add, 2), "")))
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
    assertEquals(primary("123"), Some(PResult(123, "")))
    assertEquals(primary("12+3"), Some(PResult(12, "+3")))
    assertEquals(primary("123"), Some(PResult(123, "")))
    assertEquals(primary("123abc"), Some(PResult(123, "abc")))
    assertEquals(primary("abc"), Some(PResult("abc", "")))
    assertEquals(primary("abc123"), Some(PResult("abc", "123")))
    assertEquals(primary("+123"), None)
    assertEquals(primary(""), None)
  }
  // test("factor") {

  // }
}
