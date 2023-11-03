package parser

import Node._

case class PResult[T](
    token: T,
    rest: String
)
type Parser[T] = String => Option[PResult[T]]

enum Node {
  case Add(lhs: Node)(rhs: Node)
  case Sub(lhs: Node)(rhs: Node)
  case Mul(lhs: Node)(rhs: Node)
  case Div(lhs: Node)(rhs: Node)
  case IntNum(n: Int)
  case Str(str: String)
  case Achar(char: Char)
  case Dummy
}
type OpChar = '+' | '-' | '*' | '/'
// type Rec[A, F] = A match
//   case Node => Node | F => Node
//   case _ => A | Rec[A, F]

// type Operater = Node => Node => Node
// type OneHadNode = Node => Node
// type NodeT = Node | OneHadNode | Operater

// type Comb[A, B] = (A, B) match {
//   case (Operater, Node)   => OneHadNode
//   case (OneHadNode, Node) => Node
// }

// def add: Operater = rhs => lhs => Add(rhs)(lhs)
// def sub: Operater = rhs => lhs => Sub(rhs)(lhs)
// def mul: Operater = rhs => lhs => Mul(rhs)(lhs)
// def div: Operater = rhs => lhs => Div(rhs)(lhs)
