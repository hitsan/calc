package parser
import Node._

type Operater = '+' | '-' | '*' | '/'

case class PResult[T](
    token: T,
    rest: String
)
type Parser[A] = String => Option[PResult[A]]

type OrParser[A, B] = Parser[A] | Parser[B]
// type OrList[A, B, C] = A match
//   case Parser[_] => Parser[List[B | C]]
//   case PResult[_] => PResult[List[B | C]]

enum Node:
  case Add(lhs: Node, rhs: Node)
  case Sub(lhs: Node, rhs: Node)
  case Mul(lhs: Node, rhs: Node)
  case Div(lhs: Node, rhs: Node)
  case IntNum(n: Int)
  case Str(str: String)
  case Achar(char: Char)
  case Bool(value: Boolean)
type OneHand = Node => Node
type TwoHand = Node => Node => Node
type Ast = Node | OneHand | TwoHand

def add: TwoHand = lhs => rhs => Add(lhs, rhs)
def sub: TwoHand = lhs => rhs => Sub(lhs, rhs)
def mul: TwoHand = lhs => rhs => Mul(lhs, rhs)
def div: TwoHand = lhs => rhs => Div(lhs, rhs)
