package parser

object Eval {
  import Node._

  extension (result: Option[PResult[Node]])
    def eval: Int = {
      val num = for {
        PResult(node, rest) <- result
      } yield calc(node)
      num.get
    }

  def calc(node: Node): Int = {
    node match {
      case Add(lhs, rhs) => calc(lhs) + calc(rhs)
      case Sub(lhs, rhs) => calc(lhs) - calc(rhs)
      case Mul(lhs, rhs) => calc(lhs) * calc(rhs)
      case Div(lhs, rhs) => calc(lhs) / calc(rhs)
      case IntNum(n)     => n
      case _             => 0
    }
  }

}
