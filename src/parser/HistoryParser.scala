package parser

import components.{Tree, Edge, Token}
import scala.collection.immutable.Stack
import vanilla.StackParser

class HistoryParser {
  var parser: StackParser = _

  def parseHistory(tree: Tree): ParseHistory = {
    parser = new StackParser(tree)

    var decisions = Seq[ParseDecision]()
    var contexts = Seq[Context]()

    while (parser.isNonTerminal) {
      val context = genContext(decisions)
      val decision = getParseDecision
      parser.applyParseDecision(decision)

      decisions :+= decision
      contexts :+= context
    }

    ParseHistory(contexts, decisions)
  }

  private def genContext(decisions: Seq[ParseDecision]) = {
    Context(parser.tree, parser.stack, parser.buffer, parser.edgeList, decisions)
  }

  /**
   * Determine true parse decision according to page 32 of Dependency Parsing
   * @return a completed parse decision object
   */
  private def getParseDecision: ParseDecision =
   if (shouldLeftReduce)
     LeftReduce(parser.buffer.top, parser.stack.top)
   else if (shouldRightReduce)
     RightReduce(parser.stack.top, parser.buffer.top)
   else
     Shift(parser.buffer.top)

  private def shouldLeftReduce = {
    !parser.stack.isEmpty && parser.tree.hasEdge(parser.buffer.top.id, parser.stack.top.id)
  }

  private def shouldRightReduce = {
    if (parser.stack.isEmpty || !parser.tree.hasEdge(parser.stack.top.id, parser.buffer.top.id))
      false
    else if (areAllOutputEdgesParsedForToken(parser.buffer.top.id))
      true
    else
      false
  }

  private def areAllOutputEdgesParsedForToken(from: Int) = {
    val edges = parser.tree.getOutputEdges(from)
    edges.forall(dep => parser.edgeList.exists(edge => {
      edge.head.id == dep.head.id && edge.dep.id == dep.dep.id
    }))
  }
}
