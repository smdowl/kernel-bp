package parser

import components.Tree
import vanilla.{AbstractStackParser, HistoryStackParser}

class HistoryParser {
  var parser: AbstractStackParser = _
  var tree: Tree = _

  private def stack = parser.stack
  private def buffer = parser.buffer
  private def edgeList = parser.edgeList

  def parseHistory(tree: Tree): ParseHistory = {
    this.tree = tree
    parser = new HistoryStackParser(tree.tokens)

    var decisions = Seq[ParseDecision]()
    var contexts = Seq[Context]()

    while (parser.isNonTerminal) {
      val context = parser.generateContext(decisions)
      val decision = getParseDecision
      parser.applyParseDecision(decision)

      decisions :+= decision
      contexts :+= context
    }

    ParseHistory(contexts, decisions)
  }

  /**
   * Determine true parse decision according to page 32 of Dependency Parsing
   * @return a completed parse decision object
   */
  private def getParseDecision: ParseDecision =
   if (shouldLeftReduce)
     LeftReduce(buffer.top, stack.top)
   else if (shouldRightReduce)
     RightReduce(stack.top, buffer.top)
   else
     Shift(buffer.top)

  private def shouldLeftReduce = {
    !stack.isEmpty && tree.hasEdge(buffer.top.id, stack.top.id)
  }

  private def shouldRightReduce = {
    if (stack.isEmpty || !tree.hasEdge(stack.top.id, buffer.top.id))
      false
    else if (areAllOutputEdgesParsedForToken(buffer.top.id))
      true
    else
      false
  }

  private def areAllOutputEdgesParsedForToken(from: Int) = {
    val edges = tree.getOutputEdges(from)
    edges.forall(dep => edgeList.exists(edge => {
      edge.head.id == dep.head.id && edge.dep.id == dep.dep.id
    }))
  }
}
