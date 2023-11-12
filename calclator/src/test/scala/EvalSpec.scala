import parser.PResult
class EvalSpec extends munit.FunSuite {
  import parser.Expression._
  import parser.Eval._

  test("add") {
    assertEquals(expression("1+2").eval, 1 + 2)
    assertEquals(expression("2 + 5").eval, 2 + 5)
  }

  test("sub") {
    assertEquals(expression("2-2").eval, 2 - 2)
    assertEquals(expression("3-2").eval, 3 - 2)
    assertEquals(expression("2 - 5").eval, 2 - 5)
  }

  test("mul") {
    assertEquals(expression("2*2").eval, 2 * 2)
    assertEquals(expression("2 * 5").eval, 2 * 5)
  }
  test("div") {
    assertEquals(expression("2/2").eval, 2 / 2)
    assertEquals(expression("2 / 5").eval, 2 / 5)
    assertEquals(expression("6 / 2").eval, 6 / 2)
  }

  test("exprssion") {
    assertEquals(expression("1+2/2*5").eval, 1 + 2 / 2 * 5)
    assertEquals(expression("22 / 5+ 5*6+3").eval, 22 / 5 + 5 * 6 + 3)
  }
}
