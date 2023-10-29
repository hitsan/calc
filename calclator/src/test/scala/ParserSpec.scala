class ParserSpec extends munit.FunSuite {
  import parser.Parser._
  import parser.ParserGenerater._
  import parser.PResult
  import parser.Ast.Node._
  import parser.Ast.Node

  test("Digit") {
    assertEquals(digit("1+1"), Some(PResult(IntNum(1), "+1")))
    assertEquals(digit(""), None)
    assertEquals(digit("a"), None)
  }

  test("Number") {
    assertEquals(intNum("12+3"), Some(PResult(IntNum(12), "+3")))
    assertEquals(intNum("+123"), None)
    assertEquals(intNum(""), None)
  }

  test("Operater") {
    val parsePlus = operater('+')
    assertEquals(parsePlus("+"), Some(PResult(add, "")))
    assertEquals(parsePlus("++"), Some(PResult(add, "+")))
    assertEquals(parsePlus("+1"), Some(PResult(add, "1")))
    assertEquals(parsePlus("1+1"), None)
    assertEquals(parsePlus(""), None)

    assertEquals(add(IntNum(1))(IntNum(1)), Add(IntNum(1))(IntNum(1)))
  }

  test("String") {
    val parser = string("if")
    assertEquals(parser("if"), Some(PResult(Str("if"), "")))
    assertEquals(parser("ifelse"), Some(PResult(Str("if"), "else")))
    assertEquals(parser("11+2"), None)
    assertEquals(parser(""), None)
  }

  test("Any string") {
    assertEquals(anyString("if"), Some(PResult(Str("if"), "")))
    assertEquals(anyString("ifelse"), Some(PResult(Str("ifelse"), "")))
    assertEquals(anyString("11+2"), None)
    assertEquals(anyString(""), None)
  }
}