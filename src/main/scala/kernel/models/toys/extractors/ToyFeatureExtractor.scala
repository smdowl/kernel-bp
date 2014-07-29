package kernel.models.toys.extractors

import breeze.linalg.{DenseVector, DenseMatrix}
import computation.FeatureVector

trait ToyFeatureExtractor {
  /**
   * Convert each set of samples into an array of feature
   */
  def extractFeatures(sampleData: Seq[String]): Seq[FeatureVector]
}
