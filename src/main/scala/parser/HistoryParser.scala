package parser

import components.Tree
import vanilla.{AbstractStackParser, HistoryStackParser}

class HistoryParser {
  var parser: AbstractStackParser = _
  var tree: Tree = _

  private def stack = parser.context.stack
  private def buffer = parser.context.buffer
  private def edgeList = parser.context.edgeList

  def parseHistory(tree: Tree): ParseHistory = {
    this.tree = tree
    parser = new HistoryStackParser(tree.tokens)

    var decisions = Seq[ParseDecision]()
    var contexts = Seq[Context]()

    while (parser.isNonTerminal) {
      val context = parser.context
      val decision = addEdgeType(getParseDecision)
      parser.applyParseDecision(decision)

      decisions :+= decision
      contexts :+= context
    }

    ParseHistory(tree.tokens, contexts, decisions)
  }

  private def addEdgeType(decision: ParseDecision): ParseDecision = decision match {
    case reduce: LeftReduce => new LeftReduce(reduce.getRoot, reduce.getDep, reduce.getRelation)
    case reduce: RightReduce => new RightReduce(reduce.getRoot, reduce.getDep, reduce.getRelation)
    case a: Any => a
  }

  /**
   * Determine true parse decision according to page 32 of Dependency Parsing
   * @return a completed parse decision object
   */
  private def getParseDecision: ParseDecision =
   if (shouldLeftReduce)
     parser.context.constructDecision(classOf[LeftReduce])
   else if (shouldRightReduce)
     parser.context.constructDecision(classOf[RightReduce])
   else
     parser.context.constructDecision(classOf[Shift])

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
