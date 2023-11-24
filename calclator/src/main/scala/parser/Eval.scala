package parser

object Eval {
  import Node._

  extension (result: Either[ParseError, Node])
    def eval: Int =
      result.map(node => calc(node)).getOrElse(0)

  extension (result: Either[ParseError, Node])
    def evalBool: Boolean =
      result.map(node => calcBool(node)).getOrElse(false)

  def calc(node: Node): Int = {
    node match {
      case Add(lhs, rhs) => calc(lhs) + calc(rhs)
      case Sub(lhs, rhs) => calc(lhs) - calc(rhs)
      case Mul(lhs, rhs) => calc(lhs) * calc(rhs)
      case Div(lhs, rhs) => calc(lhs) / calc(rhs)
      case IntNum(n)     => n
      case Negative(n)   => -calc(n)
      case _             => 0
    }
  }

  def calcBool(node: Node): Boolean = {
    node match {
      case Equals(lhs, rhs)       => calc(lhs) == calc(rhs)
      case NotEquals(lhs, rhs)    => calc(lhs) != calc(rhs)
      case Less(lhs, rhs)         => calc(lhs) < calc(rhs)
      case LessEqual(lhs, rhs)    => calc(lhs) <= calc(rhs)
      case Greater(lhs, rhs)      => calc(lhs) > calc(rhs)
      case GreaterEqual(lhs, rhs) => calc(lhs) >= calc(rhs)
      case Bool(value)            => value
      case Bang(rhs)              => !calcBool(rhs)
      case _                      => false
    }
  }

}
