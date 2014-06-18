package dependencies.features.extractors

import dependencies.parser.Context
import dependencies.components.Token
import computation.FeatureVector

class DPPaperFeatureExtractor extends FeatureExtractor {
  override def extractFeatures(context: Context): FeatureVector = {
    val output = new FeatureVector

    val stack = context.stack
    val buffer = context.buffer
    val edges = context.edgeList

    def addWord(key: String, token: Token, index: Int) = {
      output.add(s"$key-$index:word:${token.form}")
    }

    def addTag(key: String, token: Token, index: Int) = {
      output.add(s"$key-$index:tag:${token.POS}")
    }

    def getChildren(token: Token) = edges.filter(_.head.equals(token)).sortBy(_.dep)

    def addChildTag(key: String, token: Token, index: Int) = {
      val children = getChildren(token)

      if (children.size > 0)
        output.add(s"$key-$index:leftchild-tag:${children(0).dep.POS}")

      if (children.size > 0)
        output.add(s"$key-$index:rightchild-tag:${children.last.dep.POS}")
    }

    for ((token, i) <- stack.take(3).zipWithIndex) addWord("stack", token, i)
    for ((token, i) <- buffer.take(1).zipWithIndex) addWord("buffer", token, i)

    for ((token, i) <- stack.take(2).zipWithIndex) addTag("stack", token, i)
    for ((token, i) <- buffer.take(2).zipWithIndex) addWord("buffer", token, i)

    for ((token, i) <- stack.take(2).zipWithIndex) addChildTag("stack", token, i)

    output
  }
}
