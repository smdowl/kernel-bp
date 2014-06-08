package features.labellers

import parser.ParseDecision
import scala.collection.immutable.TreeSet

class EdgeOnlyDataLabeller extends DataLabeller {

  var labelSet: TreeSet[ParseDecision] = new TreeSet()

  override def initialise(decisions: Iterable[ParseDecision]): Unit = {
    decisions.foreach(labelSet += _)
  }

  override def label(decision: ParseDecision): Int = {
    val label = labelSet.until(decision).size
    assert(label < labelSet.size, "Should contain element")
    label
  }

  override def getInstance(label: Int): ParseDecision = labelSet.drop(label).head
}
