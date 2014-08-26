package kernel.models.toyextractors

import computation.FeatureVector

class TrigramFeatureExtractor extends ToyFeatureExtractor {
  /**
   * Convert each set of samples into an array of feature
   */
  override def extractFeatures(sampleData: Seq[String]): Seq[FeatureVector] = {
    val hidden = sampleData.slice(0, sampleData.length / 2)
    val visible = sampleData.slice(sampleData.length / 2, sampleData.length)

    var prev: String = null
    var prevPrev: String = null


    hidden.map(node => {
      val vector = new FeatureVector()
      vector.add(s"label:$node")

      if (prevPrev != null)
        vector.add(s"prev-prev-label:$prevPrev")

      if (prev != null) {
        vector.add(s"prev-label:$prev")
        prevPrev = prev
      }

      prev = node
      vector
    }) ++ visible.map(node => {
      val vector = new FeatureVector()
      vector.add(s"form:$node")
      vector
    })
  }
}
