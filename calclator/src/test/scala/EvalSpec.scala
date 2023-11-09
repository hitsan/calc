class EvalSpec extends munit.FunSuite {
  import parser.Eval._
  import parser.PResult
  import parser.Token._
  import parser.Primitive._
  import parser.Combinator._

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
    assertEquals(factor("123"), Some(PResult(IntNum(123), "")))
    assertEquals(factor("1*2"), Some(PResult(Mul(IntNum(1), IntNum(2)), "")))
    assertEquals(factor("1/2"), Some(PResult(Div(IntNum(1), IntNum(2)), "")))
    assertEquals(
      factor("1*2*3"),
      Some(PResult(Mul(Mul(IntNum(1), IntNum(2)), IntNum(3)), ""))
    )
    assertEquals(
      factor("1*2/3"),
      Some(PResult(Div(Mul(IntNum(1), IntNum(2)), IntNum(3)), ""))
    )
    assertEquals(
      factor("1*2/3*4"),
      Some(
        PResult(Mul(Div(Mul(IntNum(1), IntNum(2)), IntNum(3)), IntNum(4)), "")
      )
    )

    assertEquals(factor("*2/3*4"), None)
    assertEquals(factor(""), None)
    assertEquals(factor("1+2/3*4"), Some(PResult(IntNum(1), "+2/3*4")))
  }

  test("term") {
    assertEquals(term("123"), Some(PResult(IntNum(123), "")))
    assertEquals(term("1+2"), Some(PResult(Add(IntNum(1), IntNum(2)), "")))
    assertEquals(term("1-2"), Some(PResult(Sub(IntNum(1), IntNum(2)), "")))
    assertEquals(
      term("1+2-3"),
      Some(PResult(Sub(Add(IntNum(1), IntNum(2)), IntNum(3)), ""))
    )
    assertEquals(
      term("1-2+3"),
      Some(PResult(Add(Sub(IntNum(1), IntNum(2)), IntNum(3)), ""))
    )
    assertEquals(
      term("1+2-3+4"),
      Some(
        PResult(Add(Sub(Add(IntNum(1), IntNum(2)), IntNum(3)), IntNum(4)), "")
      )
    )
    assertEquals(term("+23*4"), None)
    assertEquals(term(""), None)

    assertEquals(
      term("1+2*3"),
      Some(PResult(Add(IntNum(1), Mul(IntNum(2), IntNum(3))), ""))
    )
    assertEquals(
      term("1/2-3"),
      Some(PResult(Sub(Div(IntNum(1), IntNum(2)), IntNum(3)), ""))
    )
    assertEquals(
      term("1*2-3/4"),
      Some(
        PResult(Sub(Mul(IntNum(1), IntNum(2)), Div(IntNum(3), IntNum(4))), "")
      )
    )
  }
}
