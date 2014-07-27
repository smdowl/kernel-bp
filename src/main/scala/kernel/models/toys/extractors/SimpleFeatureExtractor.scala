package kernel.models.toys.extractors

import computation.FeatureVector

class SimpleFeatureExtractor extends ToyFeatureExtractor {
  /**
   * Convert each set of samples into an array of feature
   */
  override def extractFeatures(sampleData: Iterable[String]): Seq[FeatureVector] = {
    sampleData.map(node => {
      val vector = new FeatureVector()
      vector.add(s"label:$node")
      vector
    }).toSeq
  }
}
