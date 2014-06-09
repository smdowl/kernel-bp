package parser

import components.{EmptyToken, Token}

abstract class ParseDecision extends Ordered[ParseDecision] {
  override def compare(that: ParseDecision): Int = {
    this.hashCode() compare that.hashCode()
  }

  def emptyDecision: ParseDecision = this match {
    case LeftReduce(root, dep) =>
      LeftReduce(new EmptyToken, new EmptyToken)

    case RightReduce(root, dep) =>
      RightReduce(new EmptyToken, new EmptyToken)

    case Shift(token) =>
      Shift(new EmptyToken)
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