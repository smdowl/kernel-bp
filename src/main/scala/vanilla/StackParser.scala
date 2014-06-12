package vanilla

import components._
import scala.collection.immutable.Stack
import parser._
import parser.classifiers.Classifier
import components.Token

abstract class AbstractStackParser(tokens: Seq[Token]) {
  protected var currentContext: Context = initContext()

  private def initContext() = {
    Context(Stack(tokens(0)), Stack() ++ tokens.drop(1), Seq(), Seq())
  }

  def context = currentContext

  def isNonTerminal = !context.buffer.isEmpty

  def applyParseDecision(decision: ParseDecision) = {
    currentContext = currentContext.applyParseDecision(decision)
  }
}

class HistoryStackParser(tokens: Seq[Token]) extends AbstractStackParser(tokens)

class StackParser(tokens: Seq[Token], classifier: Classifier) extends AbstractStackParser(tokens) {

  def parseSentence(): Tree = {
    while (isNonTerminal) {
      val decision = getParseDecision
      applyParseDecision(decision)
    }

    new Tree(tokens, context.edgeList)
  }

  private def getParseDecision = classifier.getParseDecision(currentContext)
}
