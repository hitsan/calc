package parser
import Token._

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

enum Token:
  case Add(lhs: Token, rhs: Token)
  case Sub(lhs: Token, rhs: Token)
  case Mul(lhs: Token, rhs: Token)
  case Div(lhs: Token, rhs: Token)
  case IntNum(n: Int)
  case Str(str: String)
  case Achar(char: Char)
type OneHand = Token => Token
type TwoHand = Token => Token => Token
type Node = Token | OneHand | TwoHand

def add: TwoHand = lhs => rhs => Add(lhs, rhs)
def sub: TwoHand = lhs => rhs => Sub(lhs, rhs)
def mul: TwoHand = lhs => rhs => Mul(lhs, rhs)
def div: TwoHand = lhs => rhs => Div(lhs, rhs)
