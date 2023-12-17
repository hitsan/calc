package parser
import Node._

case class PResult[T](
    token: T,
    rest: String
)
type Parser[A] = String => Option[PResult[A]]

type OrParser[A, B] = Parser[A] | Parser[B]
// type OrList[A, B, C] = A match
//   case Parser[_] => Parser[List[B | C]]
//   case PResult[_] => PResult[List[B | C]]

case class ParseError(message: String) extends Exception(message)
case class RuntimeError(message: String) extends Exception(message)

case class Stmt(
    expr: Node,
    rest: String
)
enum Node:
  case Add(lhs: Node, rhs: Node)
  case Sub(lhs: Node, rhs: Node)
  case Mul(lhs: Node, rhs: Node)
  case Div(lhs: Node, rhs: Node)
  case Bang(rhs: Node)
  case IntNum(n: Int)
  case Negative(n: Node)
  case Str(str: String)
  case Achar(char: Char)
  case Bool(value: Boolean)
  case LParentheses
  case RParentheses
  case Equals(lhs: Node, rhs: Node)
  case NotEquals(lhs: Node, rhs: Node)
  case Greater(lhs: Node, rhs: Node)
  case GreaterEqual(lhs: Node, rhs: Node)
  case Less(lhs: Node, rhs: Node)
  case LessEqual(lhs: Node, rhs: Node)
type OneHand = Node => Node
type TwoHand = Node => Node => Node
type Ast = Node | OneHand | TwoHand
