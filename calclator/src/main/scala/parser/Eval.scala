package parser

object Eval {
  import Node._

  type Value = Int | Boolean | String
  type Result = Either[RuntimeError, Value]

  extension (result: Either[ParseError, Node])
    def eval: Either[RuntimeError, Value] = result match {
      case Left(value)  => Left(RuntimeError(value.message))
      case Right(value) => calc(value)
    }

  def binNumExpr(
      op: (Int, Int) => Int,
      lNode: Node,
      rNode: Node
  ): Either[RuntimeError, Value] = {
    val lhs = calc(lNode)
    val rhs = calc(rNode)
    for {
      l <- lhs
      r <- rhs
      ret <- (l, r) match {
        case (l: Int, r: Int) => Right(op(l, r))
        case (l: Int, r) =>
          Left(RuntimeError(s"Expected Integer, but found $r"))
        case (l, r: Int) =>
          Left(RuntimeError(s"Expected Integer, but found $l"))
        case (l, r) =>
          Left(RuntimeError(s"Expected Integer, but found $l and $r"))
      }
    } yield ret
  }

  def binBoolExpr(
      op: (Int, Int) => Boolean,
      lNode: Node,
      rNode: Node
  ): Either[RuntimeError, Value] = {
    val lhs = calc(lNode)
    val rhs = calc(rNode)
    for {
      l <- lhs
      r <- rhs
      ret <- (l, r) match {
        case (l: Int, r: Int) => Right(op(l, r))
        case (l: Int, r) =>
          Left(RuntimeError(s"Expected Integer, but found $r"))
        case (l, r: Int) =>
          Left(RuntimeError(s"Expected Integer, but found $l"))
        case (l, r) =>
          Left(RuntimeError(s"Expected Integer, but found $l and $r"))
      }
    } yield ret
  }

  def monoNumExpr(op: (Int) => Int, node: Node): Either[RuntimeError, Value] = {
    val rhs = calc(node)
    for {
      r <- rhs
      ret <- r match {
        case r: Int => Right(op(r))
        case r      => Left(RuntimeError(s"Expected Integer, but found $r"))
      }
    } yield ret
  }

  def monoBoolExpr(
      op: (Boolean) => Boolean,
      node: Node
  ): Either[RuntimeError, Value] = {
    val rhs = calc(node)
    for {
      r <- rhs
      ret <- r match {
        case r: Boolean => Right(op(r))
        case r          => Left(RuntimeError(s"Expected Boolean, but found $r"))
      }
    } yield ret
  }

  def calc(node: Node): Either[RuntimeError, Value] =
    node match {
      case Add(lhs, rhs)          => binNumExpr(add, lhs, rhs)
      case Sub(lhs, rhs)          => binNumExpr(sub, lhs, rhs)
      case Mul(lhs, rhs)          => binNumExpr(mul, lhs, rhs)
      case Div(lhs, rhs)          => binNumExpr(div, lhs, rhs)
      case Equals(lhs, rhs)       => binBoolExpr(equals, lhs, rhs)
      case NotEquals(lhs, rhs)    => binBoolExpr(notEquals, lhs, rhs)
      case Less(lhs, rhs)         => binBoolExpr(less, lhs, rhs)
      case LessEqual(lhs, rhs)    => binBoolExpr(lessEqual, lhs, rhs)
      case Greater(lhs, rhs)      => binBoolExpr(greater, lhs, rhs)
      case GreaterEqual(lhs, rhs) => binBoolExpr(greaterEqual, lhs, rhs)
      case Negative(rhs)          => monoNumExpr(negative, rhs)
      case Bang(rhs)              => monoBoolExpr(bang, rhs)
      case IntNum(n)              => Right(n)
      case Bool(n)                => Right(n)
      case _                      => Left(RuntimeError("Invalid syntax"))
    }
  val add = (a: Int, b: Int) => a + b
  val sub = (a: Int, b: Int) => a - b
  val mul = (a: Int, b: Int) => a * b
  val div = (a: Int, b: Int) => a / b
  val equals = (a: Int, b: Int) => a == b
  val notEquals = (a: Int, b: Int) => a != b
  val less = (a: Int, b: Int) => a < b
  val lessEqual = (a: Int, b: Int) => a <= b
  val greater = (a: Int, b: Int) => a > b
  val greaterEqual = (a: Int, b: Int) => a >= b
  val bang = (a: Boolean) => !a
  val negative = (a: Int) => -a
}
