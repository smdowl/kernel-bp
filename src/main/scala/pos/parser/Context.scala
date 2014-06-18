package pos.parser

import scala.collection.immutable.Stack

class Context(val history: Stack[String] = new Stack[String]()) {
  def add(pos: String) = new Context(history :+ pos)
}
