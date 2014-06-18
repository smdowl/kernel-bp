package pos.parser

class Context(val history: Seq[String] = Seq[String]()) {
  def add(pos: String) = new Context(history :+ pos)
}
