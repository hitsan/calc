class LexerSpec extends munit.FunSuite {
  import TestData._
  import Lexer._

  test("determine test") {
    var tok = determine('+')
    assertEquals(tok.get, p)
    tok = determine('-')
    assertEquals(tok.get, h)
    tok = determine('*')
    assertEquals(tok.get, a)
    tok = determine('/')
    assertEquals(tok.get, s)
    tok = determine('1')
    assertEquals(tok.get, n1)
    tok = determine('2')
    assertEquals(tok.get, n2)
    tok = determine('3')
    assertEquals(tok.get, n3)
    tok = determine(' ')
    assertEquals(tok, None)
  }

  test("Lexer test") {
    (lex("1 + 2") zip List(n1, p, n2))
      .foreach(r => assertEquals(r._1, r._2))
    (lex("2 - 1") zip List(n2, h, n1))
      .foreach(r => assertEquals(r._1, r._2))
    (lex("3 * 4") zip List(n3, a, n4))
      .foreach(r => assertEquals(r._1, r._2))
    (lex("4 / 2") zip List(n4, s, n2))
      .foreach(r => assertEquals(r._1, r._2))
    (lex("4 + 2 - 1") zip List(n4, p, n2, h, n1))
      .foreach(r => assertEquals(r._1, r._2))
    (lex("11") zip List(n11))
      .foreach(r => assertEquals(r._1, r._2))
    (lex("123 + 11") zip List(n123, p, n11))
      .foreach(r => assertEquals(r._1, r._2))
  }

}
