import parser.PResult
class EvalSpec extends munit.FunSuite {
  import parser.Expression._
  import parser.Eval._
  import parser.Parser._

  test("add") {
    assertEquals(parse("1+2").eval, 1 + 2)
    assertEquals(parse("2 + 5").eval, 2 + 5)
  }

  test("sub") {
    assertEquals(parse("2-2").eval, 2 - 2)
    assertEquals(parse("3-2").eval, 3 - 2)
    assertEquals(parse("2 - 5").eval, 2 - 5)
  }

  test("mul") {
    assertEquals(parse("2*2").eval, 2 * 2)
    assertEquals(parse("2 * 5").eval, 2 * 5)
  }
  test("div") {
    assertEquals(parse("2/2").eval, 2 / 2)
    assertEquals(parse("2 / 5").eval, 2 / 5)
    assertEquals(parse("6 / 2").eval, 6 / 2)
  }

  test("exprssion") {
    assertEquals(parse("1+2/2*5").eval, 1 + 2 / 2 * 5)
    assertEquals(parse("22 / 5+ 5*6+3").eval, 22 / 5 + 5 * 6 + 3)
    assertEquals(parse("22 - (5+ 5)*6+3").eval, 22 - (5 + 5) * 6 + 3)
    assertEquals(
      parse("22 * (5 + (4 + 5)*6)+3").eval,
      22 * (5 + (4 + 5) * 6) + 3
    )
    assertEquals(parse("1---2").eval, 1 - (-(-2)))
  }

  test("equals") {
    assertEquals(parse("1 == 1").evalBool, true)
    assertEquals(parse("2 == 3").evalBool, false)
  }

  test("notEquals") {
    assertEquals(parse("1 != 1").evalBool, false)
    assertEquals(parse("2 != 3").evalBool, true)
  }

  test("less") {
    assertEquals(parse("1 < 2").evalBool, true)
    assertEquals(parse("2 < 1").evalBool, false)
  }

  test("lessEqual") {
    assertEquals(parse("1 <= 2").evalBool, true)
    assertEquals(parse("2 <= 2").evalBool, true)
    assertEquals(parse("3 <= 2").evalBool, false)
  }

  test("greater") {
    assertEquals(parse("2 > 1").evalBool, true)
    assertEquals(parse("1 > 2").evalBool, false)
  }

  test("greaterEqual") {
    assertEquals(parse("2 >= 1").evalBool, true)
    assertEquals(parse("2 >= 2").evalBool, true)
    assertEquals(parse("2 >= 3").evalBool, false)
  }

  test("bool") {
    assertEquals(parse("true").evalBool, true)
    assertEquals(parse("false").evalBool, false)
  }

  // test("bang") {
  //   assertEquals(parse("!true").evalBool, false)
  //   assertEquals(parse("!false").evalBool, true)
  // }
}
