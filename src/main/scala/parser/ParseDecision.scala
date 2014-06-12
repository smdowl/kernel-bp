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

abstract class ParseDecision

abstract class Reduce(root: Token, dep: Token, relation: String, direction: String) extends ParseDecision {

  override def equals(that: Any) = that match {
    case reduce: Reduce =>
      this.direction.equals(reduce.getDirection) &&
      this.root.equals(reduce.getRoot) &&
      this.dep.equals(reduce.getDep) &&
      this.relation.equals(reduce.getRelation)
    case _ =>
      false
  }

  def getRoot = root
  def getDep = dep
  def getDirection = direction
  def getRelation = relation

  override def hashCode = (17 * (41 + root.hashCode) * (dep.hashCode + 39 * direction.hashCode)) * relation.hashCode

  override def toString = {
    s"$direction shift to add ${root.form} -$relation-> ${dep.form}"
  }
}

case class LeftReduce(root: Token, dep: Token, relation: String = "_") extends Reduce(root, dep, relation, "left")

case class RightReduce(root: Token, dep: Token, relation: String = "_") extends Reduce(root, dep, relation, "right")

case class Shift(token: Token) extends ParseDecision {
  override def toString = {
    s"Shift ${token.form}"
  }
}

class TestDecision extends ParseDecision

object Reduce extends App {
  val left = ParseDecision.getEmptyDecision(classOf[LeftReduce])
  val left2 = ParseDecision.getEmptyDecision(classOf[LeftReduce])
  val right = ParseDecision.getEmptyDecision(classOf[RightReduce])

  println(left.equals(left2))
  println(left.equals(right))
}