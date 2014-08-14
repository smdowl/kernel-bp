package kernel.models.toys.extractors

import computation.FeatureVector

import scala.collection.mutable.Queue

class ComplexToyFeaturesExtractor extends ToyFeatureExtractor {
  /**
   * Convert each set of samples into an array of feature
   */
  override def extractFeatures(sampleData: Seq[String]): Seq[FeatureVector] = {
    val hidden = sampleData.slice(0, sampleData.length / 2)
    val visible = sampleData.slice(sampleData.length / 2, sampleData.length)

    val history = Queue[String]()

    hidden.map(node => {
      val vector = new FeatureVector()
      vector.add(s"label:$node")
      vector.add(s"history:$history")
      history.enqueue(node)

      if (history.length > 3)
        history.dequeue()
      vector
    }) ++ visible.map(node => {
      val vector = new FeatureVector()
      vector.add(s"form:$node")
      vector
    })
  }
}
