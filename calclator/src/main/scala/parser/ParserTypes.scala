package parser
import Node._

case class PResult[T](
    token: T,
    rest: String
)
type Parser[T] = String => Option[PResult[T]]
type OpChar = '+' | '-' | '*' | '/'
enum Node:
  case Add(lhs: Node, rhs: Node)
  case Sub(lhs: Node, rhs: Node)
  case Mul(lhs: Node, rhs: Node)
  case Div(lhs: Node, rhs: Node)
  case IntNum(n: Int)
  case Str(str: String)
  case Achar(char: Char)
def add: TwoHand = rhs => lhs => Add(rhs, lhs)
def sub: TwoHand = rhs => lhs => Sub(rhs, lhs)
def mul: TwoHand = rhs => lhs => Mul(rhs, lhs)
def div: TwoHand = rhs => lhs => Div(rhs, lhs)

type OneHand = Node => Node
type TwoHand = Node => Node => Node
type Ast[A] = A match
  case Node => Node | OneHand | TwoHand
  case _    => A | A => A | A => A => A

type Rec[A] = A match
  case Node => Node | List[Rec[Node]]
  case _    => A | List[Rec[A]]
