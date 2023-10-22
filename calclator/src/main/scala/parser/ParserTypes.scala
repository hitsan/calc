package parser

import Ast.Node
import Ast.Node._

case class PResult[+T](
    token: T,
    rest: String
)
type Parser[T] = String => Option[PResult[T]]
type Token = Node | Char | Int | Oprater | String
type Oprater = Add.type | Sub.type | Mul.type | Div.type
