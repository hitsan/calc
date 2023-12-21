class ExpressionSpec extends munit.FunSuite {
  import parser.Expression._
  import parser.PResult
  import parser.Node._

  test("primary") {
    assertEquals(expression("123"), Some(PResult(IntNum(123), "")))
    assertEquals(primary("12 + 3"), Some(PResult(IntNum(12), " + 3")))
    assertEquals(primary("123abc"), Some(PResult(IntNum(123), "abc")))
    assertEquals(primary("abc"), Some(PResult(Str("abc"), "")))
    assertEquals(primary("abc123"), Some(PResult(Str("abc"), "123")))
    assertEquals(primary("+123"), None)
    assertEquals(primary(""), None)
  }

  test("factor") {
    assertEquals(factor("123"), Some(PResult(IntNum(123), "")))
    assertEquals(factor("1 * 2"), Some(PResult(Mul(IntNum(1), IntNum(2)), "")))
    assertEquals(factor("1 / 2"), Some(PResult(Div(IntNum(1), IntNum(2)), "")))
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

  test("expression") {
    assertEquals(expression("123"), Some(PResult(IntNum(123), "")))
    assertEquals(
      expression("1+2"),
      Some(PResult(Add(IntNum(1), IntNum(2)), ""))
    )
    assertEquals(
      expression("1-2"),
      Some(PResult(Sub(IntNum(1), IntNum(2)), ""))
    )
    assertEquals(
      expression("1+2-3"),
      Some(PResult(Sub(Add(IntNum(1), IntNum(2)), IntNum(3)), ""))
    )
    assertEquals(
      expression("1-2+3"),
      Some(PResult(Add(Sub(IntNum(1), IntNum(2)), IntNum(3)), ""))
    )
    assertEquals(
      expression("1 + 2 - 3 + 4"),
      Some(
        PResult(Add(Sub(Add(IntNum(1), IntNum(2)), IntNum(3)), IntNum(4)), "")
      )
    )
    assertEquals(expression("+23*4"), None)
    assertEquals(expression(""), None)

    assertEquals(
      expression("1+2*3"),
      Some(PResult(Add(IntNum(1), Mul(IntNum(2), IntNum(3))), ""))
    )
    assertEquals(
      expression("1/2-3"),
      Some(PResult(Sub(Div(IntNum(1), IntNum(2)), IntNum(3)), ""))
    )
    assertEquals(
      expression("1*2-3/4"),
      Some(
        PResult(Sub(Mul(IntNum(1), IntNum(2)), Div(IntNum(3), IntNum(4))), "")
      )
    )
    assertEquals(
      expression("(1+2)*3"),
      Some(PResult(Mul(Add(IntNum(1), IntNum(2)), IntNum(3)), ""))
    )
    assertEquals(
      expression("1/(2-3)"),
      Some(PResult(Div(IntNum(1), Sub(IntNum(2), IntNum(3))), ""))
    )
    assertEquals(
      expression("(1+2)*(3-4)"),
      Some(
        PResult(Mul(Add(IntNum(1), IntNum(2)), Sub(IntNum(3), IntNum(4))), "")
      )
    )
    assertEquals(
      expression("(1+2)*3/4"),
      Some(
        PResult(Div(Mul(Add(IntNum(1), IntNum(2)), IntNum(3)), IntNum(4)), "")
      )
    )
    assertEquals(expression("-1"), Some(PResult(Negative(IntNum(1)), "")))
    assertEquals(
      expression("-1+2"),
      Some(PResult(Add(Negative(IntNum(1)), IntNum(2)), ""))
    )
    assertEquals(
      expression("1+ -2"),
      Some(PResult(Add(IntNum(1), Negative(IntNum(2))), ""))
    )
    assertEquals(
      expression("1* -2"),
      Some(PResult(Mul(IntNum(1), Negative(IntNum(2))), ""))
    )
  }

  test("expression with negative numbers in polynomials") {
    assertEquals(
      expression("1+(-2)+3"),
      Some(PResult(Add(Add(IntNum(1), Negative(IntNum(2))), IntNum(3)), ""))
    )
    assertEquals(
      expression("1-(-2)-3"),
      Some(PResult(Sub(Sub(IntNum(1), Negative(IntNum(2))), IntNum(3)), ""))
    )
    assertEquals(
      expression("1*(-2)*3"),
      Some(PResult(Mul(Mul(IntNum(1), Negative(IntNum(2))), IntNum(3)), ""))
    )
    assertEquals(
      expression("1/(-2)/3"),
      Some(PResult(Div(Div(IntNum(1), Negative(IntNum(2))), IntNum(3)), ""))
    )
    assertEquals(
      expression("1+2*(-3)"),
      Some(PResult(Add(IntNum(1), Mul(IntNum(2), Negative(IntNum(3)))), ""))
    )
    assertEquals(
      expression("1*2+(-3)"),
      Some(PResult(Add(Mul(IntNum(1), IntNum(2)), Negative(IntNum(3))), ""))
    )
    assertEquals(expression(""), None)
    assertEquals(expression("+23*4"), None)

  }

  test("bool expression") {
    assertEquals(expression("true"), Some(PResult(Bool(true), "")))
    assertEquals(expression("false"), Some(PResult(Bool(false), "")))
    assertEquals(expression("!false"), Some(PResult(Bang(Bool(false)), "")))
    assertEquals(
      expression("!!false"),
      Some(PResult(Bang(Bang(Bool(false))), ""))
    )
  }

  test("comparison operators") {
    assertEquals(
      expression("1 < 2"),
      Some(PResult(Less(IntNum(1), IntNum(2)), ""))
    )
    assertEquals(
      expression("1 > 2"),
      Some(PResult(Greater(IntNum(1), IntNum(2)), ""))
    )
    assertEquals(
      expression("1 <= 2"),
      Some(PResult(LessEqual(IntNum(1), IntNum(2)), ""))
    )
    assertEquals(
      expression("1 >= 2"),
      Some(PResult(GreaterEqual(IntNum(1), IntNum(2)), ""))
    )
    assertEquals(
      expression("1 == 2"),
      Some(PResult(Equals(IntNum(1), IntNum(2)), ""))
    )
    assertEquals(
      expression("1 != 2"),
      Some(PResult(NotEquals(IntNum(1), IntNum(2)), ""))
    )
  }

  test("exprStmt") {
    assertEquals(exprStmt("1;"), Some(PResult(IntNum(1), "")))
    assertEquals(exprStmt("1+2;"), Some(PResult(Add(IntNum(1), IntNum(2)), "")))
    assertEquals(
      exprStmt("1+2-3;"),
      Some(PResult(Sub(Add(IntNum(1), IntNum(2)), IntNum(3)), ""))
    )
    assertEquals(
      exprStmt("1+2-3+4;"),
      Some(
        PResult(Add(Sub(Add(IntNum(1), IntNum(2)), IntNum(3)), IntNum(4)), "")
      )
    )
    assertEquals(exprStmt("+23*4;"), None)
    assertEquals(exprStmt(""), None)

    assertEquals(
      exprStmt("1+2*3;"),
      Some(PResult(Add(IntNum(1), Mul(IntNum(2), IntNum(3))), ""))
    )
    assertEquals(
      exprStmt("1/2-3;"),
      Some(PResult(Sub(Div(IntNum(1), IntNum(2)), IntNum(3)), ""))
    )
    assertEquals(
      exprStmt("1*2-3/4;"),
      Some(
        PResult(Sub(Mul(IntNum(1), IntNum(2)), Div(IntNum(3), IntNum(4))), "")
      )
    )
    assertEquals(
      exprStmt("(1+2)*3;"),
      Some(PResult(Mul(Add(IntNum(1), IntNum(2)), IntNum(3)), ""))
    )
    assertEquals(
      exprStmt("1/(2-3);"),
      Some(PResult(Div(IntNum(1), Sub(IntNum(2), IntNum(3))), ""))
    )
    assertEquals(
      exprStmt("(1+2)*(3-4);"),
      Some(
        PResult(Mul(Add(IntNum(1), IntNum(2)), Sub(IntNum(3), IntNum(4))), "")
      )
    )
    assertEquals(
      exprStmt("(1+2)*3/4;"),
      Some(
        PResult(Div(Mul(Add(IntNum(1), IntNum(2)), IntNum(3)), IntNum(4)), "")
      )
    )
    assertEquals(exprStmt("-1;"), Some(PResult(Negative(IntNum(1)), "")))
  }
  // test("expression with invalid characters") {
  //   // assertEquals(expression("1a"), None)
  //   assertEquals(expression("1+*2"), None)
  //   assertEquals(expression("1++2"), None)
  //   assertEquals(expression("1..2"), None)
  //   assertEquals(expression("1/0"), None)
  // }

  // test("expression with missing operands") {
  //   assertEquals(expression("1+"), None)
  //   assertEquals(expression("*2"), None)
  //   assertEquals(expression("/"), None)
  // }

  // test("expression with unbalanced parentheses") {
  //   assertEquals(expression("(1+2"), None)
  //   assertEquals(expression("1+2)"), None)
  //   assertEquals(expression("((1+2)"), None)
  //   assertEquals(expression("(1+2))"), None)
  // }

  // test("expression with invalid unary operators") {
  //   assertEquals(expression("!1"), None)
  //   assertEquals(expression("-"), None)
  //   assertEquals(expression("!"), None)
  // }

  // test("comparison operators - invalid expressions") {
  //   assertEquals(expression("1 <"), None)
  //   assertEquals(expression("> 2"), None)
  //   assertEquals(expression("1 <= 2 >"), None)
  // assertEquals(
  //   expression("1 >= 2 =="),
  //   None
  // )
  // assertEquals(
  //   expression("1 !="),
  //   None
  // )
  // }
}
