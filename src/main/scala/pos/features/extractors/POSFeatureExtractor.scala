package pos.features.extractors

import pos.parser.{ParseHistory, Context}
import computation.FeatureVector

trait POSFeatureExtractor {
  def extractFeatures(history: ParseHistory): Seq[FeatureVector]
}
