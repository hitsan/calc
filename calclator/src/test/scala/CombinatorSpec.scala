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

    val intStr = or(intNum, anyString)
    assertEquals(intStr("+2"), None)
    assertEquals(intStr("-2"), None)
    assertEquals(intStr("*2"), None)
    assertEquals(intStr("a+2"), Some(PResult(Str("a"), "+2")))
    assertEquals(intStr("ab+2"), Some(PResult(Str("ab"), "+2")))
    assertEquals(intStr("11-2"), Some(PResult(IntNum(11), "-2")))
    assertEquals(intStr(""), None)

    val charStr = or(anyChar, anyString)
    assertEquals(charStr("a+2"), Some(PResult(Achar('a'), "+2")))
    assertEquals(charStr("ab+2"), Some(PResult(Achar('a'), "b+2")))

    val strChar = or(anyString, anyChar)
    assertEquals(strChar("a+2"), Some(PResult(Str("a"), "+2")))
    assertEquals(strChar("ab+2"), Some(PResult(Str("ab"), "+2")))

    val str = or(string("if"), string("else"))
    assertEquals(str("ifelse"), Some(PResult(Str("if"), "else")))
    assertEquals(str("elseif"), Some(PResult(Str("else"), "if")))
    assertEquals(str("eiflseif"), None)
    assertEquals(str("ab+2"), None)
  }

  test("structAst") {
    val parser = and(intNum, operater('+'), intNum).struct(astRule)

    assertEquals(
      parser("1+2"),
      Some(PResult(Add(IntNum(1), IntNum(2)), ""))
    )
    assertEquals(
      parser("1+2+3"),
      Some(PResult(Add(IntNum(1), IntNum(2)), "+3"))
    )
    assertEquals(
      parser("11+2"),
      Some(PResult(Add(IntNum(11), IntNum(2)), ""))
    )
    assertEquals(parser("a+2"), None)
    assertEquals(parser("11-2"), None)
    assertEquals(parser(""), None)

    val parser1 =
      and(intNum, or(operater('-'), operater('+')), intNum).struct(astRule)
    assertEquals(
      parser1("1+2"),
      Some(PResult(Add(IntNum(1), IntNum(2)), ""))
    )
    assertEquals(
      parser1("1-2"),
      Some(PResult(Sub(IntNum(1), IntNum(2)), ""))
    )
    assertEquals(parser1("1*2"), None)

    val parser2 = and(intNum, rep0(and(operater('+'), intNum))).struct(astRule)
    assertEquals(
      parser2("1+2+3"),
      Some(PResult(Add(Add(IntNum(1), IntNum(2)), IntNum(3)), ""))
    )
    assertEquals(
      parser2("1+2a"),
      Some(PResult(Add(IntNum(1), IntNum(2)), "a"))
    )
    assertEquals(parser2("1-2"), Some(PResult(IntNum(1), "-2")))
    assertEquals(parser2("1"), Some(PResult(IntNum(1), "")))
    assertEquals(parser2(""), None)
    assertEquals(parser2("a+a"), None)
  }
}
