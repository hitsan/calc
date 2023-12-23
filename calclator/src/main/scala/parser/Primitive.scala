package parser

object Primitive {
  import Node._
  import PResult._
  import Combinator._

  // Don't allow space
  def anyCharS: Parser[Node] = code =>
    code.headOption.withFilter(_.isLetter).map { head =>
      PResult(Achar(head), code.tail)
    }

  def digitS: Parser[Node] = code =>
    code.headOption.withFilter(_.isDigit).map { head =>
      PResult(IntNum(head.asDigit), code.tail)
    }

  def anyStringS: Parser[Node] = code => {
    def joinChars(chars: List[Node]): Node = chars.foldLeft(Str("")) {
      (str, char) =>
        (str, char) match {
          case (Str(str), Achar(char)) => Str(str + char)
          case _                       => sys.error("Invalid token")
        }
    }
    rep(anyChar)(code).map { case PResult(tokens, rest) =>
      PResult(joinChars(tokens), rest)
    }
  }

  def identifierS: Parser[Node] = code => {
    def joinName(chars: List[Node]): Node = chars.foldLeft(Identifier("")) {
      (str, char) =>
        (str, char) match {
          case (Identifier(str), Achar(char)) => Identifier(str + char)
          case _                       => sys.error("Invalid token")
        }
    }
    rep(anyChar)(code).map { case PResult(tokens, rest) =>
      PResult(joinName(tokens), rest)
    }
  }

  def intNumS: Parser[Node] = code => {
    def joinIntNums(nums: List[Node]): Node = nums.foldLeft(IntNum(0)) {
      (acc, num) =>
        (acc, num) match {
          case (IntNum(n1), IntNum(n2)) => IntNum(10 * n1 + n2)
          case _                        => sys.error("Invalid token")
        }
    }
    rep(digitS)(code).map { case PResult(tokens, rest) =>
      PResult(joinIntNums(tokens), rest)
    }
  }

  def charS(character: Char): Parser[Node] = code =>
    code.headOption.withFilter(_ == character).map { head =>
      PResult(Achar(head), code.tail)
    }

  def stringS(word: String): Parser[Node] = code =>
    if (code.startsWith(word)) Some(PResult(Str(word), code.drop(word.length)))
    else None

  val binOpMap: Map[Node, TwoHand] = Map(
    Achar('+') -> ((lhs: Node) => (rhs: Node) => Add(lhs, rhs)),
    Achar('-') -> ((lhs: Node) => (rhs: Node) => Sub(lhs, rhs)),
    Achar('*') -> ((lhs: Node) => (rhs: Node) => Mul(lhs, rhs)),
    Achar('/') -> ((lhs: Node) => (rhs: Node) => Div(lhs, rhs)),
    Achar('>') -> ((lhs: Node) => (rhs: Node) => Greater(lhs, rhs)),
    Achar('<') -> ((lhs: Node) => (rhs: Node) => Less(lhs, rhs)),
    Str("==") -> ((lhs: Node) => (rhs: Node) => Equals(lhs, rhs)),
    Str("!=") -> ((lhs: Node) => (rhs: Node) => NotEquals(lhs, rhs)),
    Str(">=") -> ((lhs: Node) => (rhs: Node) => GreaterEqual(lhs, rhs)),
    Str("<=") -> ((lhs: Node) => (rhs: Node) => LessEqual(lhs, rhs))
  )

  def binOperater(op: Char): Parser[TwoHand] = code =>
    for {
      PResult(token, rest) <- charS(op)(code)
      operation <- binOpMap.get(token)
    } yield PResult(operation, rest)

  def binOperater(op: String): Parser[TwoHand] = code =>
    for {
      PResult(token, rest) <- stringS(op)(code)
      operation <- binOpMap.get(token)
    } yield PResult(operation, rest)

  val monoOpMap: Map[Node, OneHand] = Map(
    Achar('!') -> ((rhs: Node) => Bang(rhs)),
    Achar('-') -> ((rhs: Node) => Negative(rhs))
  )

  def monoOperater(op: Char): Parser[OneHand] = code =>
    for {
      PResult(token, rest) <- charS(op)(code)
      operation <- monoOpMap.get(token)
    } yield PResult(operation, rest)

  def boolS: Parser[Node] = code =>
    (parseBool("true") | parseBool("false"))(code)

  def parseBool(bool: "true" | "false"): Parser[Node] = code =>
    stringS(bool)(code).map { case PResult(value, rest) =>
      PResult(Bool(bool.toBoolean), rest)
    }

  val keyMap: Map[Node, Node] = Map(
    Achar('(') -> LParentheses,
    Achar(')') -> RParentheses,
    Achar(';') -> Semicolon,
    Str("var") -> Var,
    Achar('=') -> Assign
  )

  def keyword(word: Char): Parser[Node] = code =>
    for {
      PResult(token, rest) <- charS(word)(code)
      keyword <- keyMap.get(token)
    } yield PResult(keyword, rest)

  def keyword(word: String): Parser[Node] = code =>
    for {
      PResult(token, rest) <- stringS(word)(code)
      keyword <- keyMap.get(token)
    } yield PResult(keyword, rest)

  def skipSpace[A](parser: Parser[A]): Parser[A] =
    code => parser(code.trim)

  // Blow functions can skip space
  def anyChar = skipSpace(anyCharS)
  def digit = skipSpace(digitS)
  def anyString = skipSpace(anyStringS)
  def intNum = skipSpace(intNumS)
  def char(character: Char) = skipSpace(charS(character))
  def string(word: String) = skipSpace(stringS(word))
  def plus = skipSpace(binOperater('+'))
  def minus = skipSpace(binOperater('-'))
  def times = skipSpace(binOperater('*'))
  def divide = skipSpace(binOperater('/'))
  def bang = skipSpace(monoOperater('!'))
  def bool = skipSpace(boolS)
  def negative = skipSpace(monoOperater('-'))
  def lParentheses = skipSpace(keyword('('))
  def rParentheses = skipSpace(keyword(')'))
  def equal = skipSpace(binOperater("=="))
  def notEqual = skipSpace(binOperater("!="))
  def greater = skipSpace(binOperater('>'))
  def greaterEqual = skipSpace(binOperater(">="))
  def less = skipSpace(binOperater('<'))
  def lessEqual = skipSpace(binOperater("<="))
  def semicolon = skipSpace(keyword(';'))
  def varKey = skipSpace(keyword("var"))
  def identifier = skipSpace(identifierS)
  def assign = skipSpace(keyword('='))
}
