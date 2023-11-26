import parser.PResult
class EvalSpec extends munit.FunSuite {
  import parser.Expression._
  import parser.Eval._
  import parser.Parser._
  import parser.RuntimeError

  test("add") {
    assertEquals(parse("1+2").eval, Right(1 + 2))
    assertEquals(parse("2 + 5").eval, Right(2 + 5))
  }

  test("sub") {
    assertEquals(parse("2-2").eval, Right(2 - 2))
    assertEquals(parse("3-2").eval, Right(3 - 2))
    assertEquals(parse("2 - 5").eval, Right(2 - 5))
  }

  test("mul") {
    assertEquals(parse("2*2").eval, Right(2 * 2))
    assertEquals(parse("2 * 5").eval, Right(2 * 5))
  }

  test("div") {
    assertEquals(parse("2/2").eval, Right(2 / 2))
    assertEquals(parse("2 / 5").eval, Right(2 / 5))
    assertEquals(parse("6 / 2").eval, Right(6 / 2))
  }

  test("exprssion") {
    assertEquals(parse("1+2/2*5").eval, Right(1 + 2 / 2 * 5))
    assertEquals(parse("22 / 5+ 5*6+3").eval, Right(22 / 5 + 5 * 6 + 3))
    assertEquals(parse("22 - (5+ 5)*6+3").eval, Right(22 - (5 + 5) * 6 + 3))
    assertEquals(
      parse("22 * (5 + (4 + 5)*6)+3").eval,
      Right(22 * (5 + (4 + 5) * 6) + 3)
    )
    assertEquals(parse("1---2").eval, Right(1 - (-(-2))))
  }

  test("equals") {
    assertEquals(parse("1 == 1").eval, Right(true))
    assertEquals(parse("2 == 3").eval, Right(false))
  }

  test("notEquals") {
    assertEquals(parse("1 != 1").eval, Right(false))
    assertEquals(parse("2 != 3").eval, Right(true))
  }

  test("less") {
    assertEquals(parse("1 < 2").eval, Right(true))
    assertEquals(parse("2 < 1").eval, Right(false))
  }

  test("lessEqual") {
    assertEquals(parse("1 <= 2").eval, Right(true))
    assertEquals(parse("2 <= 2").eval, Right(true))
    assertEquals(parse("3 <= 2").eval, Right(false))
  }

  test("greater") {
    assertEquals(parse("2 > 1").eval, Right(true))
    assertEquals(parse("1 > 2").eval, Right(false))
  }

  test("greaterEqual") {
    assertEquals(parse("2 >= 1").eval, Right(true))
    assertEquals(parse("2 >= 2").eval, Right(true))
    assertEquals(parse("2 >= 3").eval, Right(false))
  }

  test("bool") {
    assertEquals(parse("true").eval, Right(true))
    assertEquals(parse("false").eval, Right(false))
  }

  test("bang") {
    assertEquals(parse("!true").eval, Right(false))
    assertEquals(parse("!false").eval, Right(true))
    assertEquals(parse("!!true").eval, Right(true))
    assertEquals(parse("!!false").eval, Right(false))
    assertEquals(parse("!(1 < 2)").eval, Right(false))
  }

  test("Abnormal case") {
    assertEquals(parse("2 + true").eval, Left(RuntimeError("Expected Integer, but found true")))
    assertEquals(parse("false + 1").eval, Left(RuntimeError("Expected Integer, but found false")))
    assertEquals(parse("false + true").eval, Left(RuntimeError("Expected Integer, but found false and true")))
    assertEquals(parse("-false + 1").eval, Left(RuntimeError("Expected Integer, but found false")))
    assertEquals(parse("!1").eval, Left(RuntimeError("Expected Boolean, but found 1")))
  }
}
