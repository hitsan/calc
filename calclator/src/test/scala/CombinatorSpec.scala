class CombinatorSpec extends munit.FunSuite {
  import parser.PResult
  import parser.Primitive._
  import parser.Combinator._
  import parser.Node._

  test("repeat") {
    val number = rep(digit)
    assertEquals(
      number("123"),
      Some(PResult(List(IntNum(1), IntNum(2), IntNum(3)), ""))
    )
    assertEquals(
      number("123+12"),
      Some(PResult(List(IntNum(1), IntNum(2), IntNum(3)), "+12"))
    )
    assertEquals(number(""), None)
  }

  test("repeat Zero or More") {
    val number = rep0(digit)
    assertEquals(
      number("123"),
      Some(PResult(List(IntNum(1), IntNum(2), IntNum(3)), ""))
    )
    assertEquals(
      number("123+12"),
      Some(PResult(List(IntNum(1), IntNum(2), IntNum(3)), "+12"))
    )
    assertEquals(number(""), Some(PResult(Nil, "")))
    assertEquals(
      number("+12"),
      Some(PResult(List(), "+12"))
    )
  }

  test("and") {
    val parser = and(intNum, char('+'), intNum)
    assertEquals(
      parser("1+2"),
      Some(PResult(List(IntNum(1), Achar('+'), IntNum(2)), ""))
    )
    assertEquals(
      parser("1+2+3"),
      Some(PResult(List(IntNum(1), Achar('+'), IntNum(2)), "+3"))
    )
    assertEquals(
      parser("11+2"),
      Some(PResult(List(IntNum(11), Achar('+'), IntNum(2)), ""))
    )
    assertEquals(parser("a+2"), None)
    assertEquals(parser("11-2"), None)
    assertEquals(parser(""), None)
  }

  test("or") {
    val op = or(char('-'), char('+'))
    assertEquals(op("+2"), Some(PResult(Achar('+'), "2")))
    assertEquals(op("-2"), Some(PResult(Achar('-'), "2")))
    assertEquals(op("*2"), None)
    assertEquals(op("a+2"), None)
    assertEquals(op("11-2"), None)
    assertEquals(op(""), None)

    val intStr = or(intNum, anyLetter)
    assertEquals(intStr("+2"), None)
    assertEquals(intStr("-2"), None)
    assertEquals(intStr("*2"), None)
    assertEquals(intStr("a+2"), Some(PResult(Achar('a'), "+2")))
    assertEquals(intStr("ab+2"), Some(PResult(Achar('a'), "b+2")))
    assertEquals(intStr("11-2"), Some(PResult(IntNum(11), "-2")))
    assertEquals(intStr(""), None)

    val charStr = or(anyChar, anyLetter)
    assertEquals(charStr("a+2"), Some(PResult(Achar('a'), "+2")))
    assertEquals(charStr("ab+2"), Some(PResult(Achar('a'), "b+2")))

    val str = or(string("if"), string("else"))
    assertEquals(str("ifelse"), Some(PResult(Str("if"), "else")))
    assertEquals(str("elseif"), Some(PResult(Str("else"), "if")))
    assertEquals(str("eiflseif"), None)
    assertEquals(str("ab+2"), None)
  }
}
