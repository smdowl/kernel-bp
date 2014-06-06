package components

class Tree(val tokens: Seq[Token], val edges: Seq[Edge]) {
  def getRoot = tokens(0)
  def getToken(id: Int) = tokens(id)
  def getNonRootTokens = tokens.tail

  def hasEdge(from: Int, to: Int) = getEdge(from, to).isDefined
  def getEdge(from: Int, to: Int): Option[Edge] = {
    edges.find(dep => dep.head.id == from && dep.dep.id == to)
  }
  def getOutputEdges(from: Int) = edges.filter(dep => {
    dep.head.id == from
  })

  def iterator = tokens.iterator
}