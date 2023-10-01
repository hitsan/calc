object Parser {
  import Token.Token._
  import Token.Token
  import Ast.Node._
  import Ast.Node

  def isOperand(token: Token): Boolean = token match {
    case a: Num => false
    case _      => true
  }

  def isNum(token: Token): Boolean = token match {
    case a: Num => true
    case _      => false
  }

  def parseInt(token: Token): Node = token match {
    case Num(n) => Integer(n)
    case _      => Integer(0)
  }

  def parseExpression(tokens: List[Token]): Node = {
    val lhs = if (isNum(tokens.head)) {
      parseInt(tokens.head)
    } else {
      Integer(0)
    }
    tokens.drop(1)
    val ope = tokens.head
    if (isOperand(ope)) {
      tokens.drop(1)
      val rhs = parseExpression(tokens)
      ope match {
        case Plus     => Add(lhs, rhs)
        case Hyphen   => Sub(lhs, rhs)
        case Astarisk => Mul(lhs, rhs)
        case Slash    => Div(lhs, rhs)
        case _        => Add(lhs, rhs)
      }
    } else {
      Integer(0)
    }

  }

  def parse(tokens: List[Token]): Node = {
    tokens.head match {
      case Num(n)   => parseInt(tokens.head)
      case Plus     => ???
      case Hyphen   => ???
      case Astarisk => ???
      case Slash    => ???
      case null     => ???
    }
  }
}
