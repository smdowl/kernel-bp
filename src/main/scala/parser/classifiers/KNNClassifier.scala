package parser.classifiers

import parser.{ParseHistory, Trainable, ParseDecision, Context}
import features.{DataLabeller, DataPoint, FeatureExtractor}
import scala.collection.mutable

class KNNClassifier(k: Int, extractor: FeatureExtractor, labeller: DataLabeller) extends Classifier with Trainable {

  var data: Seq[DataPoint] = _

  override def getParseDecision(context: Context): ParseDecision =
  {
    if (data.isEmpty)
      throw new Exception("Not been initialised with data!")

    val features = extractor.extractFeatures(context)

    val best = new mutable.PriorityQueue[ScoredDataInstance]()

    data.foreach(point => {
      val score = features.distance(point.x)
      val scoredInstance = ScoredDataInstance(point, score)

      best += scoredInstance
    })

    // TODO: Currently doing 1-NN
    labeller.getInstance(best.head.vector.y)
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

