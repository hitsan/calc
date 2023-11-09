class EvalSpec extends munit.FunSuite {
  import parser.Eval._
  import parser.PResult
  import parser.Token._
  import parser.Parser._
  import parser.ParserGenerater._

  test("primary") {
    assertEquals(primary("123"), Some(PResult(IntNum(123), "")))
    assertEquals(primary("12+3"), Some(PResult(IntNum(12), "+3")))
    assertEquals(primary("123abc"), Some(PResult(IntNum(123), "abc")))
    assertEquals(primary("abc"), Some(PResult(Str("abc"), "")))
    assertEquals(primary("abc123"), Some(PResult(Str("abc"), "123")))
    assertEquals(primary("+123"), None)
    assertEquals(primary(""), None)
  }

  test("factor") {
    val p = applyExpr(factor)(makeAst)
    assertEquals(p("123"), Some(PResult(IntNum(123), "")))
    assertEquals(p("1*2"), Some(PResult(Mul(IntNum(1),IntNum(2)), "")))
    assertEquals(p("1/2"), Some(PResult(Div(IntNum(1),IntNum(2)), "")))
    assertEquals(p("1*2*3"), Some(PResult(Mul(Mul(IntNum(1),IntNum(2)), IntNum(3)), "")))
    assertEquals(p("1*2/3"), Some(PResult(Div(Mul(IntNum(1),IntNum(2)), IntNum(3)), "")))
    assertEquals(p("1*2/3*4"), Some(PResult(Mul(Div(Mul(IntNum(1),IntNum(2)), IntNum(3)), IntNum(4)), "")))

    assertEquals(p("*2/3*4"), None)
    assertEquals(p(""), None)
    assertEquals(p("1+2/3*4"), Some(PResult(IntNum(1), "+2/3*4")))
  }
}
