package parser.classifiers

import parser.{ParseHistory, Trainable, ParseDecision, Context}
import features.DataPoint
import scala.collection.mutable
import features.extractors.FeatureExtractor
import features.labellers.DataLabeller
import scala.util.Random

class KNNClassifier(k: Int, extractor: FeatureExtractor, labeller: DataLabeller) extends Classifier with Trainable {

  var data: Seq[DataPoint] = _

  private val KNNOrdering = new Ordering[ScoredDataInstance]() {
    def compare(a: ScoredDataInstance, b: ScoredDataInstance): Int = {
      - (a.score compareTo b.score)
    }
  }
  override def getParseDecision(context: Context): ParseDecision =
  {
    if (data.isEmpty)
      throw new Exception("Not been initialised with data!")

    val features = extractor.extractFeatures(context)

    val best = new mutable.PriorityQueue[ScoredDataInstance]()(KNNOrdering)

    data.foreach(point => {
      val score = features.distance(point.x)
      val scoredInstance = ScoredDataInstance(point, score)

      best += scoredInstance
    })

    getDecision(best)
  }

  private def getDecision(best: mutable.PriorityQueue[ScoredDataInstance]) = {
    var counts: Map[Int, Int] = Map()
    best.take(k).foreach(instance => {
      counts = count(counts, instance)
    })
    labeller.getInstance(getHighest(counts))
  }

  private def count(counts: Map[Int, Int], toAdd: ScoredDataInstance) = {
    val label = toAdd.vector.y
    counts + (label -> (counts.getOrElse(label, 0) + 1))
  }

  private def getHighest(counts: Map[Int, Int]) = {
    val sorted = counts.toSeq.sortBy(_._2)
    val max = sorted.filter(_._2 == sorted(0)._2)

    val labels = max.map(_._1)

    // TODO: Not chose randomly?
    Random.shuffle(labels).toList.head
  }

  override def train(histories: Seq[ParseHistory]): Unit = {

    val decisions = histories.flatMap(_.parseDecisions)
    labeller.initialise(decisions)

    val labels = decisions.map(labeller.label)

    val features = histories.flatMap(history => {
      history.contexts.map(extractor.extractFeatures)
    })

    data = (features zip labels).map(tup => DataPoint(tup._1, tup._2))
  }
}

