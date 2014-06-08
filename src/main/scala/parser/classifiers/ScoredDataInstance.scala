package parser.classifiers

import features.DataPoint

case class ScoredDataInstance(vector: DataPoint, score: Double) extends Ordered[ScoredDataInstance] {
  override def compare(that: ScoredDataInstance): Int = this.score compare that.score
}
