object Token {
  enum Token {
    case Plus
    case Hyphen
    case Astarisk
    case Slash
    case Num(n: Int)
  }
}

object Lexer {
  import Token.Token._
  import Token.Token

  def determine(s: Char): Option[Token] = s match {
    case '+'              => Some(Plus)
    case '-'              => Some(Hyphen)
    case '*'              => Some(Astarisk)
    case '/'              => Some(Slash)
    case n if (s.isDigit) => Some(Num(n.asDigit))
    case ' '              => None
  }

  def lex(line: String): List[Token] = {
    val ret = line.flatMap(s => determine(s)).toList
    ret.tail.foldLeft(List(ret.head)) { (acc, x) =>
      (acc.last, x) match {
        case (Num(s), Num(n)) => acc.init ++ List(Num(s * 10 + n))
        case _                => acc ++ List(x)
      }
    }
  }

}
