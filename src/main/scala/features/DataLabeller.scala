package features

import parser.ParseDecision

trait DataLabeller {
  def initialise(decisions: Iterable[ParseDecision])
  def label(decision: ParseDecision): Int
  def getInstance(label: Int): ParseDecision
}
