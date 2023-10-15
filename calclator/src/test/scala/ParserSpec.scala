class ParserSpec extends munit.FunSuite {
  import Parser._
  import Ast.Node._
  import Ast.Node

  // test("Parse Integer") {
  //   assertEquals(parseInt("1"), Some(PResult(Integer(1), "")))
  //   assertEquals(parseInt("12"), Some(PResult(Integer(12), "")))
  //   assertEquals(parseInt("123"), Some(PResult(Integer(123), "")))
  //   assertEquals(parseInt("11 22"), Some(PResult(Integer(11), " 22")))
  //   assertEquals(
  //     parseInt("12 23 34"),
  //     Some(PResult(Integer(12), " 23 34"))
  //   )
  //   assertEquals(parseInt("e"), None)
  //   assertEquals(parseInt("abc"), None)
  //   assertEquals(parseInt("a123"), None)
  //   assertEquals(parseInt("aa 123"), None)
  //   assertEquals(parseInt(""), None)
  // }

  // test("Ignore space") {
  //   def parserInt = skipSpace(parseInt)
  //   assertEquals(parserInt(" 1"), Some(PResult(Integer(1), "")))
  //   assertEquals(parserInt("  12  23"), Some(PResult(Integer(12), "  23")))
  // }

  // test("Parse String") {
  //   def parseIfWord = parseString("if")
  //   assertEquals(parseIfWord("if 1+2"), Some(PResult(Id("if"), " 1+2")))
  //   assertEquals(parseIfWord("aif 1+2"), None)
  //   assertEquals(parseIfWord("1+2"), None)
  // }

  // test("term") {
  //   assertEquals(primary("1"), Some(PResult(Integer(1), "")))
  // }

  // test("nums") {
  //   val input = "1+1"
  //   val a = for {
  //     PResult(c, r) <- parseInt(input)
  //     PResult(c1, r1) <- parsePlus(r)
  //     PResult(c2, r2) <- parseInt(r1)
  //   } yield PResult(c1(c)(c2), r2)
  //   assertEquals(a, Some(PResult(Add(Integer(1))(Integer(1)), "")))
  // }

  // Initial
  test("isDigit") {
    assertEquals(parseDigit("1+1"), Some(PResult('1', "+1")))
    assertEquals(parseDigit(""), None)
    assertEquals(parseDigit("a"), None)
  }
}
