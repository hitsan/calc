class ParserGeneraterSpec extends munit.FunSuite {
  import parser._
  import parser.Parser._
  import parser.ParserGenerater._
  import parser.PResult
  import parser.Token._
  import parser.Token
  import parser._

  test("repeat") {
    val number = repeat(digit)
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
    val number = repeat(true)(digit)
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

  test("chain") {
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

  test("makeAst"){
    val nodes = List[Node](IntNum(1), add, IntNum(2), add, IntNum(3))
    assertEquals(makeAst(nodes), Add(Add(IntNum(1), IntNum(2)), IntNum(3)))
  }

  // test("applyExpr") {
  //   val p = chain(intNum, operater('+'), intNum)
  //   val parser = applyExpr(p)(makeAst)

  //   assertEquals(
  //     parser("1+2"),
  //     Some(PResult(Add(IntNum(1))(IntNum(2)), ""))
  //   )
  //   assertEquals(
  //     parser("1+2+3"),
  //     Some(PResult(Add(IntNum(1))(IntNum(2)), "+3"))
  //   )
  //   assertEquals(
  //     parser("11+2"),
  //     Some(PResult(Add(IntNum(11))(IntNum(2)), ""))
  //   )
  //   assertEquals(parser("a+2"), None)
  //   assertEquals(parser("11-2"), None)
  //   assertEquals(parser(""), None)
  // }
  // test("and") {
  //   val parser = and(comb1)(intNum, and(comb2)(and(comb)(operater('+'), intNum), and(comb)(operater('+'), intNum)))
  //   // assertEquals(parser("1+2"), Some(PResult(Add(IntNum(2))(IntNum(1)), "")))
  //   assertEquals(parser("1+2+3"), Some(PResult(Add(IntNum(2))(IntNum(1)), "")))
  //   assertEquals(parser("a+2"), None)
  //   assertEquals(parser("11-2"), None)
  //   assertEquals(parser(""), None)
  // }
}
