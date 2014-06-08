package parser

import components.Token

abstract class ParseDecision

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