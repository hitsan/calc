package environment

object Environment {
  import parser.Node
  import scala.collection.mutable.Map

  private val values = Map.empty[String, Node]
  def setValue(name: String, value: Node): Unit = values += (name -> value)
  def getValue(name: String): Node = values(name)
}
