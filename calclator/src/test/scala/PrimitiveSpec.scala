class PrimitiveSpec extends munit.FunSuite {
  import parser.PResult
  import parser.Primitive._
  import parser.Node._

  test("Char") {
    val p = char('a')
    assertEquals(p("  a"), Some(PResult(Achar('a'), "")))
    assertEquals(p("aa"), Some(PResult(Achar('a'), "a")))
    assertEquals(p("1+1"), None)
    assertEquals(p(""), None)
  }

  test("Digit") {
    assertEquals(digit("  1+1"), Some(PResult(IntNum(1), "+1")))
    assertEquals(digit("1+1"), Some(PResult(IntNum(1), "+1")))
    assertEquals(digit(""), None)
    assertEquals(digit("a"), None)
  }

  test("Number") {
    assertEquals(intNum("12+3"), Some(PResult(IntNum(12), "+3")))
    assertEquals(intNum("  12+3"), Some(PResult(IntNum(12), "+3")))
    assertEquals(intNum("1 2+3"), Some(PResult(IntNum(1), " 2+3")))
    assertEquals(intNum("+123"), None)
    assertEquals(intNum(""), None)
  }

  // Cannot parse only operater
  test("Operater") {
    val parsePlus = operater('+')
    assertEquals(parsePlus("1+1"), None)
    assertEquals(parsePlus(""), None)
  }

  test("String") {
    val parser = string("if")
    assertEquals(parser("if"), Some(PResult(Str("if"), "")))
    assertEquals(parser("ifelse"), Some(PResult(Str("if"), "else")))
    assertEquals(parser("  ifelse"), Some(PResult(Str("if"), "else")))
    assertEquals(parser("if else"), Some(PResult(Str("if"), " else")))
    assertEquals(parser("11+2"), None)
    assertEquals(parser(""), None)
  }

  test("Any string") {
    assertEquals(anyString("if"), Some(PResult(Str("if"), "")))
    assertEquals(anyString("ifelse"), Some(PResult(Str("ifelse"), "")))
    assertEquals(anyString("11+2"), None)
    assertEquals(anyString(""), None)
  }

  test("boolean") {
    assertEquals(bool("true"), Some(PResult(Bool(true), "")))
    assertEquals(bool("false"), Some(PResult(Bool(false), "")))
    assertEquals(bool("  true"), Some(PResult(Bool(true), "")))
    assertEquals(bool("  true 12"), Some(PResult(Bool(true), " 12")))
    assertEquals(bool("if"), None)
  }

  // test("bang") {
  //   assertEquals(bang("if"), None)
  // }
}
