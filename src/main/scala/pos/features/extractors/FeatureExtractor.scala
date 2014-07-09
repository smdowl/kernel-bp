package pos.features.extractors

import pos.parser.{ParseHistory, Context}
import computation.FeatureVector

trait FeatureExtractor {
  def extractFeatures(history: ParseHistory): Seq[FeatureVector]
}
