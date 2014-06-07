package parser

import components.{Edge, Token}
import scala.collection.immutable.Stack

case class Context(stack: Stack[Token],
                   buffer: Stack[Token],
                   edgeList: Seq[Edge],
                   decisions: Seq[ParseDecision]) {

}
