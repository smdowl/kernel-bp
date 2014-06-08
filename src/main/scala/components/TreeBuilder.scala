package components

import input.{DepParser, ConllParser, ParseToken}
import main.Constants

object TreeBuilder {
  def buildTestTree(): Tree = {
    val builder = new TreeBuilder(new ConllParser)
    builder.buildTreesFromFile(Constants.DEP_TEST)(0)
  }
}

class TreeBuilder(parser: DepParser) {
  private var depSentence: Iterable[ParseToken] = _
  private var tokens: Seq[Token] = _
  private var deps: Seq[Edge] = _

  def buildTreesFromFile(filepath: String): Seq[Tree] = {
    val depSentences = parser.parseLines(filepath)
    depSentences.map(buildTree)
  }

  private def buildTree(depSentence: Iterable[ParseToken]): Tree = {
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
