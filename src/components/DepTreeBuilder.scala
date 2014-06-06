package components

import input.ParseToken

object DepTreeBuilder {
  var depSentence: Iterable[ParseToken] = _
  var tokens: Seq[Token] = _
  var deps: Seq[Dep] = _

  def buildTree(depSentence: Iterable[ParseToken]): DepTree = {
    this.depSentence = depSentence
    init()
    parseSentence()

    new DepTree(tokens, deps)
  }

  private def init() = {
    tokens = Seq(new RootToken())
    deps = Seq()
  }

  private def parseSentence() = {
    // Iterate once over to create nodes
    tokens = tokens ++ genNodes
    parseDeps()
  }

  private def genNodes = depSentence.map(depToken => {
    Token(depToken.id, depToken.form,
      depToken.lemma, depToken.coarsePOS,
      depToken.POS, depToken.features)
  })

  private def parseDeps() = depSentence.foreach(depToken => {
    deps :+= Dep(tokens(depToken.head), tokens(depToken.id), depToken.depRel)
  })
}
