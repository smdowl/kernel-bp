package components

case class Edge(head: Token, dep: Token, relation: String) {
  override def toString = {
    s"${head.form} -> ${dep.form}"
  }
}
