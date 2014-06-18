package pos.features.extractors

import pos.parser.Context
import computation.FeatureVector

trait FeatureExtractor {
  def extractFeatures(context: Context): FeatureVector
}
