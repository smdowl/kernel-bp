package parser

import components.{DepTree, Dep, Token}
import scala.collection.immutable.{Stack, Queue}

class HistoryGenerator {
  var stack: Stack[Token] = _
  var buffer: Queue[Token] = _
  var edgeList: Seq[Dep] = _

  def genHistory(tree: DepTree): Seq[ParseDecision] = {
    stack = Stack()
    buffer = makeBuffer(tree)
    println(stack)

    Seq[ParseDecision]()
  }

  private def makeBuffer(tree: DepTree) = tree.tokens.foldLeft(Queue[Token]()) { (buffer, token) =>
    buffer.enqueue(token)
  }
}
