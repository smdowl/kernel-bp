package dependencies.features.labellers

import dependencies.parser.{ParseDecision, Context}

trait DataLabeller {
  def initialise(decisions: Iterable[ParseDecision])
  def label(decision: ParseDecision): Int
  def getInstance(context: Context, label: Int): ParseDecision
}
