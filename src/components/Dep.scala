package components

case class Dep(head: Token, dep: Token, relation: String) {
  override def toString = {
    s"${head.form} -> ${dep.form}"
  }
}
