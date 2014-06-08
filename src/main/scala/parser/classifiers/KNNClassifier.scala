package parser.classifiers

import parser.{ParseHistory, Trainable, ParseDecision, Context}
import features.{DataPoint, FeatureExtractor}
import scala.collection.mutable

class KNNClassifier(k: Int, extractor: FeatureExtractor) extends Classifier with Trainable {

  var data: Seq[DataPoint] = _
  var labelMap: Map[Int, ParseDecision] = _

  override def getParseDecision(context: Context): ParseDecision =
  {
    val features = extractor.extractFeatures(context)

    val best = new mutable.PriorityQueue[ScoredDataInstance]()

    data.foreach(point => {
      best += ScoredDataInstance(point, features.distance(point.x))
    })

    labelMap(best.head.vector.y)
  }

  override def train(histories: Seq[ParseHistory]): Unit = {
    val features = histories.flatMap(history => {
      history.contexts.map(extractor.extractFeatures)
    })

    var labels: Seq[Int] = Seq()

    histories.foreach(history => {
      history.parseDecisions.foreach(decision => {
        val label = extractor.extractLabel(decision)
        labels :+= label
        labelMap += (label -> decision)
      })
    })

    data = (features zip labels).map(tup => DataPoint(tup._1, tup._2))
  }
}

