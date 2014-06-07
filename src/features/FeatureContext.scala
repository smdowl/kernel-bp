package kernel

import parser._

object Context {

//  type FeatureExtractor =

  protected def fromDecision(decision: ParseDecision): Map[String, Int] = decision match {
    case LeftReduce(root, dep) =>
      Map("left" + root.form -> 1)

    case RightReduce(root, dep) =>
      Map("right" + root.form -> 1)

    case Shift(token) =>
      Map("shift" + token.form -> 1)

    case _ =>
      Map("test" -> 1)
  }

  def main(args: Array[String]) = {
    var context = new Context(Map("hello" -> 1))
    context = context.applyDecision(new TestDecision())
    println(context.state)

    context = context.applyDecision(new TestDecision())
    println(context.state)
  }

}

class Context(val state: Map[String, Int] = Map()) {

  def applyDecision(decision: ParseDecision): Context = {
    val decisionState = Context.fromDecision(decision)
    val newState = foldIntoContext(decisionState)

    new Context(newState)
  }

  private def foldIntoContext(decisionState: Map[String, Int]): Map[String, Int] = {
    (state.iterator ++ decisionState.iterator).foldLeft(Map[String, Int]())((map, pair) => {
      if (!map.contains(pair._1))
        map + pair
      else {
        val newMap = map - pair._1
        newMap + (pair._1 -> (map(pair._1) + pair._2))
      }
    })
  }
}
