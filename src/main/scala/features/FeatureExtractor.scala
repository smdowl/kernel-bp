package features

import parser.{Shift, RightReduce, LeftReduce, ParseDecision}
import components.{Edge, Token}
import parser.Context

trait FeatureExtractor {
  def extract(decision: ParseDecision): FeatureVector = decision match {
    case LeftReduce(root, dep) =>
      extractLeft(root, dep)

    case RightReduce(root, dep) =>
      extractRight(root, dep)

    case Shift(token) =>
      extractShift(token)

    case _ =>
      new FeatureVector(Map("test" -> 1))
  }

  def extractFeatures(context: Context): FeatureVector
  def extractLabel(decision: ParseDecision): Int

  def extractLeft(root: Token, dep: Token): FeatureVector
  def extractRight(root: Token, dep: Token): FeatureVector
  def extractShift(token: Token): FeatureVector
}