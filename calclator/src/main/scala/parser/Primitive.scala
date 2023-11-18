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

  def charToOp(node: Node): Option[TwoHand] = node match {
    case Achar('+') => Some(add)
    case Achar('-') => Some(sub)
    case Achar('*') => Some(mul)
    case Achar('/') => Some(div)
    case _          => None
  }

  def operater(op: Operater): Parser[TwoHand] = code =>
    for {
      PResult(token, rest) <- charS(op)(code)
      operation <- charToOp(token)
    } yield PResult(operation, rest)

  def bangS: Parser[OneHand] = code =>
    charS('!')(code).map { case PResult(token, rest) =>
      PResult(rhs => Bang(rhs), rest)
    }

  def negativeS: Parser[OneHand] = code =>
    charS('-')(code).map { case PResult(token, rest) =>
      PResult(num => Negative(num), rest)
    }

  def boolS: Parser[Node] = code =>
    (parseBool("true") | parseBool("false"))(code)

  def parseBool(bool: "true" | "false"): Parser[Node] = code =>
    stringS(bool)(code).map { case PResult(value, rest) =>
      PResult(Bool(bool.toBoolean), rest)
    }

  def lParenthesesS: Parser[Node] = code =>
    charS('(')(code).map { case PResult(token, rest) =>
      PResult(LParentheses, rest)
    }
  def rParenthesesS: Parser[Node] = code =>
    charS('(')(code).map { case PResult(token, rest) =>
      PResult(RParentheses, rest)
    }

  def skipSpace[A](parser: Parser[A]): Parser[A] =
    code => parser(code.trim)

  // Blow functions can skip space
  def anyChar = skipSpace(anyCharS)
  def digit = skipSpace(digitS)
  def anyString = skipSpace(anyStringS)
  def intNum = skipSpace(intNumS)
  def char(character: Char) = skipSpace(charS(character))
  def string(word: String) = skipSpace(stringS(word))
  def plus = skipSpace(operater('+'))
  def minus = skipSpace(operater('-'))
  def times = skipSpace(operater('*'))
  def divide = skipSpace(operater('/'))
  def bang = skipSpace(bangS)
  def bool = skipSpace(boolS)
  def negative = skipSpace(negativeS)
  def lParentheses = skipSpace(lParenthesesS)
  def rParentheses = skipSpace(rParenthesesS)
}
