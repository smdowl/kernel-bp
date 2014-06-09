package features.labellers

import parser.{ParseDecision, Context}

trait DataLabeller {
  def initialise(decisions: Iterable[ParseDecision])
  def label(decision: ParseDecision): Int
  def getInstance(context: Context, label: Int): ParseDecision
}
