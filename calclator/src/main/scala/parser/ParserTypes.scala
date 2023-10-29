package parser

import Ast.Node
import Ast.Node._

case class PResult[+T](
    token: T,
    rest: String
)
type Parser[T] = String => Option[PResult[T]]
// type OperaterParser[T] = String => Node => Node => Option[PResult[T]]