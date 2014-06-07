package vanilla

import components.{Edge, Token}
import scala.collection.immutable.Stack
import parser.{Shift, RightReduce, LeftReduce, ParseDecision}

class StackParser(tokens: Seq[Token]) {
  var stack: Stack[Token] = Stack(tokens(0))
  var buffer: Stack[Token] = Stack(tokens.drop(1).reverse)
  var edgeList: Seq[Edge] = Seq()

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
