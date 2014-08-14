package kernel.models.toyextractors

import computation.FeatureVector

class SimpleToyFeatureExtractor extends ToyFeatureExtractor {
  /**
   * Convert each set of samples into an array of feature
   */
  override def extractFeatures(sampleData: Seq[String]): Seq[FeatureVector] = {
    val hidden = sampleData.slice(0, sampleData.length / 2)
    val visible = sampleData.slice(sampleData.length / 2, sampleData.length)

    hidden.map(node => {
      val vector = new FeatureVector()
      vector.add(s"label:$node")
      vector
    }) ++ visible.map(node => {
      val vector = new FeatureVector()
      vector.add(s"form:$node")
      vector
    })
  }
}
