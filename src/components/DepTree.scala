package components

import input.ParseToken

class DepTree(depSentence: Seq[ParseToken]) {
  var tokens: Seq[Token] = Seq(new RootToken())
  var deps: Seq[Dep] = Seq()

  parseSentence()

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
    deps :+= Dep(getToken(depToken.head), getToken(depToken.id), depToken.depRel)
  })

  def getRoot = tokens(0)
  def getToken(id: Int) = tokens(id)
  def getNonRootTokens = tokens.tail

  def hasEdge(from: Int, to: Int) = getEdge(from, to).isDefined
  def getEdge(from: Int, to: Int): Option[Dep] = {
    deps.find(dep => dep.head.id == from && dep.dep.id == to)
  }
  def getOutputEdges(from: Int) = deps.filter(dep => {
    dep.head.id == from
  })

  def iterator = tokens.iterator
}

case class Token(id: Int,
                 form: String = "_",
                 lemma: String = "_",
                 coarsePOS: String = "_",
                 POS: String = "_",
                 features: Seq[String] = Seq())

class RootToken() extends Token(0, form="ROOT")

case class Dep(head: Token, dep: Token, relation: String) {
  override def toString = {
    s"${head.form} -> ${dep.form}"
  }
}