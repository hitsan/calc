package parser
import Token._

case class PResult[T](
    token: T,
    rest: String
)
type Parser[T] = String => Option[PResult[T]]
type Operater = '+' | '-' | '*' | '/'
enum Token:
  case Add(lhs: Token, rhs: Token)
  case Sub(lhs: Token, rhs: Token)
  case Mul(lhs: Token, rhs: Token)
  case Div(lhs: Token, rhs: Token)
  case IntNum(n: Int)
  case Str(str: String)
  case Achar(char: Char)

def add: TwoHand = rhs => lhs => Add(rhs, lhs)
def sub: TwoHand = rhs => lhs => Sub(rhs, lhs)
def mul: TwoHand = rhs => lhs => Mul(rhs, lhs)
def div: TwoHand = rhs => lhs => Div(rhs, lhs)

type OneHand = Token => Token
type TwoHand = Token => Token => Token
type TypeTpe[A] = A match
  case Token => Token | OneHand | TwoHand
  case _    => A | A => A | A => A => A

type Node = TypeTpe[Token]

type Rec[A] = A match
  case Token => Token | List[Rec[Token]]
  case _    => A | List[Rec[A]]
