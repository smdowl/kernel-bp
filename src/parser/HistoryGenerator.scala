package parser

import components.{DepTree, Dep, Token}
import scala.collection.immutable.Stack

class HistoryGenerator {
  var tree: DepTree = _
  var stack: Stack[Token] = _
  var buffer: Stack[Token] = _
  var edgeList: Seq[Dep] = _

  def generateHistory(tree: DepTree): Seq[ParseDecision] = {
    this.tree = tree
    initConfiguration()

    var decisions = Seq[ParseDecision]()

    while (isNonTerminal) {
      val decision = getParseDecision
      applyParseDecision(decision)

      decisions :+= decision
    }

    decisions
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

  /**
   * Determine true parse decision according to page 32 of Dependency Parsing
   * @return a completed parse decision object
   */
  private def getParseDecision: ParseDecision =
   if (stack.top.id != 0 && tree.hasEdge(buffer.top.id, stack.top.id))
     LeftReduce(buffer.top, stack.top)
   else if (tree.hasEdge(stack.top.id, buffer.top.id))
     RightReduce(stack.top, buffer.top)
   else
     Shift(buffer.top)

  /**
   * Apply whatever decision we are passed according to the standard rules.
   * In doing so, assert that the state of the stack and buffer are consistent
   * with the decision being applied.
   * @param decision - the decision to apply
   * @return
   */
  private def applyParseDecision(decision: ParseDecision) = decision match {
    case LeftReduce(root, dep) => {
      assert(stack.top.equals(dep))
      assert(buffer.top.equals(root))

      stack = stack.pop
      edgeList :+= Dep(root, dep, "_")
    }
    case RightReduce(root, dep) => {
      assert(stack.top.equals(root))
      assert(buffer.top.equals(dep))

      stack = stack.pop
      buffer = buffer.pop.push(root)
      edgeList :+= Dep(root, dep, "_")
    }
    case Shift(token) => {
      assert(buffer.top.equals(token))

      buffer = buffer.pop
      stack = stack.push(token)
    }
  }
}
