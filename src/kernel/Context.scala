package kernel

import parser.{TestDecision, ParseDecision}

object Context extends App {
  val context = new Context(Map("hello" -> 1))
  println(context.state)
  println(context.applyDecision(new TestDecision()).state)
}

class Context(val state: Map[String, Int] = Map()) {

  def applyDecision(decision: ParseDecision): Context = {
    val newState = updateState(decision)
    new Context(newState)
  }

  private def updateState(decision: ParseDecision) = {
    (state.iterator ++ state.iterator).foldLeft(Map[String, Int]())((map, pair) => {
      if (!map.contains(pair._1))
        map + pair
      else {
        val newMap = map - pair._1
        newMap + (pair._1 -> (map(pair._1) + pair._2))
      }
    })
  }
}
