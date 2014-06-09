package parser

import components.{EmptyToken, Edge, Token}
import scala.collection.immutable.Stack

case class Context(stack: Stack[Token],
                   buffer: Stack[Token],
                   edgeList: Seq[Edge],
                   decisions: Seq[ParseDecision]) {

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

      Context(
        stack.pop,
        buffer,
        edgeList :+ Edge(root, dep, "_"),
        decisions :+ decision
      )

    case RightReduce(root, dep) =>
      assert(stack.top.equals(root))
      assert(buffer.top.equals(dep))

      Context(
        stack.pop,
        buffer.pop.push(root),
        edgeList :+ Edge(root, dep, "_"),
        decisions :+ decision
      )

    case Shift(token) =>
      assert(buffer.top.equals(token))

      Context(
        stack.push(token),
        buffer.pop,
        edgeList,
        decisions :+ decision
      )
  }

  def constructDecision[T <: ParseDecision](cls: Class[T]): ParseDecision = {
    val emptyDecision = ParseDecision.getEmptyDecision(cls)
    fillParseDecision(emptyDecision)
  }

  def fillParseDecision(decision: ParseDecision) = decision match {
    case LeftReduce(_, _) =>
      LeftReduce(new EmptyToken, new EmptyToken)

    case RightReduce(_, _) =>
      RightReduce(new EmptyToken, new EmptyToken)

    case Shift(_) =>
      Shift(new EmptyToken)
  }
}
