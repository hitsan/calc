class ParserSpec extends munit.FunSuite {
  import Parser._
  import Ast.Expression._
  import Ast.Expression

  test("Parse Integer") {
    assertEquals(parseInt("1"), Some(PResult(Integer(1), "")))
    assertEquals(parseInt("12"), Some(PResult(Integer(12), "")))
    assertEquals(parseInt("123"), Some(PResult(Integer(123), "")))
    assertEquals(parseInt("11 22"), Some(PResult(Integer(11), " 22")))
    assertEquals(
      parseInt("12 23 34"),
      Some(PResult(Integer(12), " 23 34"))
    )
    assertEquals(parseInt("e"), None)
    assertEquals(parseInt("abc"), None)
    assertEquals(parseInt("a123"), None)
    assertEquals(parseInt("aa 123"), None)
    assertEquals(parseInt(""), None)
  }

  test("Parse Operator") {
    assertEquals(parsePlus("+2"), Some(PResult(Operator('+'), "2")))
    assertEquals(parsePlus("12+23"), None)
    assertEquals(parsePlus("12"), None)
  }

  test("Ignore space") {
    def parserInt = lexeme(parseInt)
    assertEquals(parserInt(" 1"), Some(PResult(Integer(1), "")))
    assertEquals(parserInt("  12  23"), Some(PResult(Integer(12), "  23")))
  }

  test("Parse String") {
    def parseIfWord = parseString("if")
    assertEquals(parseIfWord("if 1+2"), Some(PResult(Id("if"), " 1+2")))
    assertEquals(parseIfWord("aif 1+2"), None)
    assertEquals(parseIfWord("1+2"), None)
  }

  test("choice parser") {
    val parser = choice(parsePlus, parseMinus, parseInt, parseString("if"))
    assertEquals(parser("+2"), Some(PResult(Operator('+'), "2")))
    assertEquals(parser("123"), Some(PResult(Integer(123), "")))
    assertEquals(parser("123+"), Some(PResult(Integer(123), "+")))
    assertEquals(parser("-123"), Some(PResult(Operator('-'), "123")))
    assertEquals(parser("1+2"), Some(PResult(Integer(1), "+2")))
    assertEquals(parser("if(1)"), Some(PResult(Id("if"), "(1)")))
  }

  test("term") {
    assertEquals(term("1*1"), Some(PResult(Mul(Integer(1),Integer(1)), "")))
  }

  test("nums") {
    assertEquals(parseNums("1 1"), Some(PResult(Integer(1), "")))
  }

}
