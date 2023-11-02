package parser

import Ast.Node
import Ast.Node._

case class PResult[T](
    token: T,
    rest: String
)
type Parser[T] = String => Option[PResult[T]]
// type Rec[A, F] = A match
//   case Node => Node | F => Node
//   case _ => A | Rec[A, F]


type Operater = Node => Node => Node
type OneHadNode = Node => Node
type NodeT = Node | OneHadNode | Operater

type No[A] = A match {
  case Node => Node | Node => No[Node]
}

type Rec[F[_], A] = A match {
  case Node => Node | F[Rec[F, Node]]
  case _ => A | F[Rec[F, A]]
}

def add: Operater = rhs => lhs => Add(rhs)(lhs)
def sub: Operater = rhs => lhs => Sub(rhs)(lhs)
def mul: Operater = rhs => lhs => Mul(rhs)(lhs)
def div: Operater = rhs => lhs => Div(rhs)(lhs)
