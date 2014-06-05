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

  def getToken(id: Int) = tokens(id)

  def hasEdge(from: Int, to: Int) = getEdge(from, to).isDefined

  def getEdge(from: Int, to: Int): Option[Dep] = {
    deps.find(dep => dep.head.id == from && dep.dep.id == to)
  }

  def iterator = tokens.iterator
}

case class Token(id: Int,
                 form: String = "_",
                 lemma: String = "_",
                 coarsePOS: String = "_",
                 POS: String = "_",
                 features: Seq[String] = Seq())

class RootToken() extends Token(0)

case class Dep(head: Token, dep: Token, relation: String)