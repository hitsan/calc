class LexerSpec extends munit.FunSuite {
import Model.Token._
import Model.Token
import Lexer._

val n1 =  Num(1)
val n2 =  Num(2)
val n3 =  Num(3)
val n4 =  Num(4)
val n11 = Num(11)
val n123 = Num(123)
val a =  Add
val s =  Sub
val m =  Mul
val d =  Div

test("determine test") {
    var tok = determine('+')
    assertEquals(tok.get, a)
    tok = determine('-')
    assertEquals(tok.get, s)
    tok = determine('*')
    assertEquals(tok.get, m)
    tok = determine('/')
    assertEquals(tok.get, d)
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
    (lex("1 + 2") zip List(n1, a, n2))
        .foreach(r => assertEquals(r._1,r._2))
    (lex("2 - 1") zip List(n2, s, n1))
        .foreach(r => assertEquals(r._1, r._2))
    (lex("3 * 4") zip List(n3, m, n4))
        .foreach(r => assertEquals(r._1, r._2))
    (lex("4 / 2") zip List(n4, d, n2))
        .foreach(r => assertEquals(r._1, r._2))
    (lex("4 + 2 - 1") zip List(n4, a, n2, s, n1))
        .foreach(r => assertEquals(r._1, r._2))
    (lex("11") zip List(n11))
        .foreach(r => assertEquals(r._1, r._2))
    (lex("123 + 11") zip List(n123, a, n11))
        .foreach(r => assertEquals(r._1, r._2))
}

}
