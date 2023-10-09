class ParserSpec extends munit.FunSuite {
  import Parser._
  import Ast.Node._
  import Ast.Node

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

  test("plus calc") {
    def parsePlusCalc: Parser[Node] =
      code =>
        for {
          PResult(c, rest) <- parseInt(code)
          PResult(k, res) <- parsePlus(rest)
          PResult(v, r) <- parseInt(res)
        } yield PResult(Add(c, v), r)

    assertEquals(
      parsePlusCalc("1+2"),
      Some(PResult(Add(Integer(1), Integer(2)), ""))
    )
    assertEquals(
      parsePlusCalc("1+2+1"),
      Some(PResult(Add(Integer(1), Integer(2)), "+1"))
    )
    assertEquals(parsePlusCalc("1+"), None)
  }

  // test("mul expr") {
  //   assertEquals(parseMulExpr("1*1"), Some(PResult(Mul(Integer(1), Integer(1))), "")))
  //   // assertEquals(parseMulExpr("1*2*3"), Some(PResult(Mul(Mul(Integer(1), Integer(2))), Integer(3)))
  // }
  test("nums") {
    assertEquals(parseNums("1 1"), Some(PResult(Integer(1), "")))
  }
  // test("expr") {
  //   def pat = parsePattern(parseInt,parsePlus,parseInt,parseTimes,parseInt)
  //   assertEquals(pat("1+2*3"), Some(PResult(Integer(3), "")))
  // }

  // test("sss") {
  //   def p = parseP(parseInt,parsePlus)
  //   assertEquals(p("1+2"), Some(PResult('+', "2")))
  // }

}
