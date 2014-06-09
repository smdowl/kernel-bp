package features.labellers

import parser.{Shift, RightReduce, LeftReduce, ParseDecision, Context}
import scala.collection.immutable.TreeSet
import components.EmptyToken

class EdgeOnlyDataLabeller extends DataLabeller {

  var labelSet: TreeSet[ParseDecision] = new TreeSet()(new Ordering[ParseDecision] {
    override def compare(x: ParseDecision, y: ParseDecision): Int =
      x.hashCode() compare y.hashCode()
  })

  override def initialise(decisions: Iterable[ParseDecision]): Unit = {
    decisions.foreach(decision => {
      labelSet += ParseDecision.getEmptyDecision(decision.getClass)
    })
  }

  override def label(decision: ParseDecision): Int = {
    val label = labelSet.until(ParseDecision.getEmptyDecision(decision.getClass)).size
    assert(label < labelSet.size, "Should contain element")
    label
  }

  override def getInstance(context: Context, label: Int): ParseDecision = {
    val emptyParseDecision = labelSet.drop(label).head
    val decision = context.fillParseDecision(emptyParseDecision)
    decision
  }
}
