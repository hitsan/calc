class ParserSpec extends munit.FunSuite {
  import TestData._
  import Ast.Node._
  import Parse.parse

  test("Parse test") {
    val input = List(n1, a, n2)
    val ret = parse(input)
    assertEquals(ret, Add(Integer(1), Integer(2)))
  }
}
