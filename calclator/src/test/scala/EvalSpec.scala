class EvalSpec extends munit.FunSuite {
  import parser.Eval._
  import parser.PResult
  import parser.Token._
  import parser.Token

  import parser._
  import parser.Parser._
  import parser.ParserGenerater._
  import parser._

  test("primary") {
    val k = List[Or[Parser, Node, TwoHand]](intNum, operater('+'), intNum)
    println(k)
  }

  // test("primary") {
  //   assertEquals(primary("123"), Some(PResult(IntNum(123), "")))
  //   assertEquals(primary("12+3"), Some(PResult(IntNum(12), "+3")))
  //   assertEquals(primary("123"), Some(PResult(IntNum(123), "")))
  //   assertEquals(primary("123abc"), Some(PResult(IntNum(123), "abc")))
  //   assertEquals(primary("abc"), Some(PResult(Str("abc"), "")))
  //   assertEquals(primary("abc123"), Some(PResult(Str("abc"), "123")))
  //   assertEquals(primary("+123"), None)
  //   assertEquals(primary(""), None)
  // }

  // test("factor") {
  //   assertEquals(factor("1*2"), Some(PResult(Mul(Integer(1))(Integer(2)), "")))
  //   assertEquals(factor("1*2*3"), Some(PResult(Mul(Mul(Integer(1))(Integer(2)))(Integer(3)), "")))
  //   assertEquals(factor(""), None)
  //   assertEquals(factor("1+2"), None)
  //   assertEquals(factor("*1*2"), None)
  // }
}
