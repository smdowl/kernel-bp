package parser

import components.{Tree, Edge, Token}
import scala.collection.immutable.Stack

class HistoryParser {
  var tree: Tree = _
  var stack: Stack[Token] = _
  var buffer: Stack[Token] = _
  var edgeList: Seq[Edge] = _

  def parseHistory(tree: Tree): ParseHistory = {
    this.tree = tree
    initConfiguration()

    var decisions = Seq[ParseDecision]()
    var contexts = Seq[Context]()

    while (isNonTerminal) {
      val context = genContext(decisions)
      val decision = getParseDecision
      applyParseDecision(decision)

      decisions :+= decision
      contexts :+= context
    }

    ParseHistory(contexts, decisions)
  }

  private def initConfiguration() = {
    stack = Stack(tree.getRoot)
    buffer = refreshBuffer(tree.getNonRootTokens)
    edgeList = Seq()
  }

  private def refreshBuffer(tokens: Seq[Token]) = tokens.foldRight(Stack[Token]()) { (token, newBuffer) =>
    newBuffer.push(token)
  }

  private def isNonTerminal = !buffer.isEmpty

  private def genContext(decisions: Seq[ParseDecision]) = {
    Context(tree, stack, buffer, edgeList, decisions)
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

  /**
   * Apply whatever decision we are passed according to the standard rules.
   * In doing so, assert that the state of the stack and buffer are consistent
   * with the decision being applied.
   * @param decision - the decision to apply
   * @return
   */
  private def applyParseDecision(decision: ParseDecision) = decision match {
    case LeftReduce(root, dep) =>
      assert(stack.top.equals(dep))
      assert(buffer.top.equals(root))

      stack = stack.pop
      edgeList :+= Edge(root, dep, "_")

    case RightReduce(root, dep) =>
      assert(stack.top.equals(root))
      assert(buffer.top.equals(dep))

      stack = stack.pop
      buffer = buffer.pop.push(root)
      edgeList :+= Edge(root, dep, "_")

    case Shift(token) =>
      assert(buffer.top.equals(token))

      buffer = buffer.pop
      stack = stack.push(token)
  }
}
