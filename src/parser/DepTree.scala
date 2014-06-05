package parser

class DepTree(depSentence: Seq[ParseToken]) {
  final val ROOT = 0

  var tokens: Seq[Token] = Seq(new Token(ROOT))
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
}

case class Token(id: Int,
                 form: String = "_",
                 lemma: String = "_",
                 coarsePOS: String = "_",
                 POS: String = "_",
                 features: Seq[String] = Seq())

case class Dep(head: Token, dep: Token, relation: String)