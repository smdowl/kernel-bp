package components

import input.{Parser, ConllParser, ParsedToken}
import app.Constants

object TreeBuilder {
  def buildTestTree(): Tree = {
    val builder = new TreeBuilder(new ConllParser)
    builder.buildTreesFromFile(Constants.DEP_TEST)(0)
  }
}

class TreeBuilder(parser: Parser) {
  private var sentence: Iterable[ParsedToken] = _
  private var tokens: Seq[Token] = _
  private var deps: Seq[Edge] = _

  def buildTreesFromFile(filepath: String): Seq[Tree] = {
    val sentences = parser.parse(filepath)
    sentences.map(buildTree)
  }

  private def buildTree(sentence: Iterable[ParsedToken]): Tree = {
    this.sentence = sentence
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

  private def generateNodes = sentence.map(depToken => {
    Token(depToken.id, depToken.form,
      depToken.lemma, depToken.coarsePOS,
      depToken.POS, depToken.features)
  })

  private def parseEdges() = sentence.foreach(token => {
    deps :+= Edge(tokens(token.head), tokens(token.id), token.depRel)
  })
}
