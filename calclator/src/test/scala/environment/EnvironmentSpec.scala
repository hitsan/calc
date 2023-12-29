class EnvironmentSpec extends munit.FunSuite {
  import environment.Environment._
  import parser.Node._

  test("Set and Get Value") {
    setValue("a", IntNum(1))
    assertEquals(getValue("a"), IntNum(1))
    setValue("a", IntNum(2))
    assertEquals(getValue("a"), IntNum(2))
    setValue("b", IntNum(3))
    assertEquals(getValue("b"), IntNum(3))
    assertEquals(getValue("a"), IntNum(2))
  }
}
