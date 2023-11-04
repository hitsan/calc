package parser
import Token._

type Operater = '+' | '-' | '*' | '/'

case class PResult[T](
    token: T,
    rest: String
)
type Parser[T] = String => Option[PResult[T]]
type Or[A[_], B, C] = A[B] | A[C]

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

type Rec[A] = A match
  case Token => Token | List[Rec[Token]]
  case _     => A | List[Rec[A]]
