package features

import parser.{Shift, RightReduce, LeftReduce, ParseDecision}
import components.{Edge, Token}

trait FeatureExtractor {
  def extract(decision: ParseDecision): Map[String, Int] = decision match {
    case LeftReduce(root, dep) =>
      extractLeft(root, dep)

    case RightReduce(root, dep) =>
      extractRight(root, dep)

    case Shift(token) =>
      extractShift(token)

    case _ =>
      Map("test" -> 1)
  }

  def extractLeft(root: Token, dep: Token): Map[String,Int]
  def extractRight(root: Token, dep: Token): Map[String,Int]
  def extractShift(token: Token): Map[String,Int]
}