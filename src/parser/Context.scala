package parser

import components.{Edge, Token, Tree}
import scala.collection.immutable.Stack

case class Context(tree: Tree,
                   stack: Stack[Token],
                   buffer: Stack[Token],
                   edgeList: Seq[Edge],
                   decisions: Seq[ParseDecision]) {

}
