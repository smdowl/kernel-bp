package components

import input.ParseToken

object TreeBuilder {
  private var depSentence: Iterable[ParseToken] = _
  private var tokens: Seq[Token] = _
  private var deps: Seq[Edge] = _

  def buildTree(depSentence: Iterable[ParseToken]): Tree = {
    this.depSentence = depSentence
    init()
    parseSentence()

    new Tree(tokens, deps)
  }

  private def init() = {
    tokens = Seq(new RootToken())
    deps = Seq()
  }

  private def parseSentence() = {
    // Iterate once over to create nodes
    tokens = tokens ++ generateNodes
    parseEdges()
  }

  private def generateNodes = depSentence.map(depToken => {
    Token(depToken.id, depToken.form,
      depToken.lemma, depToken.coarsePOS,
      depToken.POS, depToken.features)
  })

  private def parseEdges() = depSentence.foreach(depToken => {
    deps :+= Edge(tokens(depToken.head), tokens(depToken.id), depToken.depRel)
  })
}
