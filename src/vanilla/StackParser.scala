package vanilla

import components.{Edge, Token, Tree}
import scala.collection.immutable.Stack
import parser.{Shift, RightReduce, LeftReduce, ParseDecision}

class StackParser(val tree: Tree) {
  var stack: Stack[Token] = Stack(tree.getRoot)
  var buffer: Stack[Token] = initBuffer(tree.getNonRootTokens)
  var edgeList: Seq[Edge] = Seq()

  private def initBuffer(tokens: Seq[Token]) = tokens.foldRight(Stack[Token]()) { (token, newBuffer) =>
    newBuffer.push(token)
  }

  def isNonTerminal = !buffer.isEmpty

  /**
   * Apply whatever decision we are passed according to the standard rules.
   * In doing so, assert that the state of the stack and buffer are consistent
   * with the decision being applied.
   * @param decision - the decision to apply
   * @return
   */
  def applyParseDecision(decision: ParseDecision) = decision match {
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
