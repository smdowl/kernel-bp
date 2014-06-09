package parser

import components.{EmptyToken, Token}

object ParseDecision {
  def getEmptyDecision[T <: ParseDecision](cls: Class[T]): ParseDecision = {
    val A = classOf[LeftReduce]
    val B = classOf[RightReduce]
    val C = classOf[Shift]

    cls match {
      case A =>
        LeftReduce(new EmptyToken, new EmptyToken)

      case B =>
        RightReduce(new EmptyToken, new EmptyToken)

      case C =>
        Shift(new EmptyToken)

    }
  }
}

abstract class ParseDecision extends Ordered[ParseDecision] {
  override def compare(that: ParseDecision): Int = {
    this.hashCode() compare that.hashCode()
  }
}

abstract class Reduce(root: Token, dep: Token) extends ParseDecision

case class LeftReduce(root: Token, dep: Token) extends Reduce(root, dep) {
  override def toString = {
    s"LeftShift to add ${root.form} -> ${dep.form}"
  }
}

case class RightReduce(root: Token, dep: Token) extends Reduce(root, dep) {
  override def toString = {
    s"RightShift to add ${root.form} -> ${dep.form}"
  }
}

case class Shift(token: Token) extends ParseDecision {
  override def toString = {
    s"Shift ${token.form}"
  }
}

class TestDecision extends ParseDecision