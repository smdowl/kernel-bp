package dependencies.features.extractors

import dependencies.parser.Context
import computation.FeatureVector

trait FeatureExtractor {
  def extractFeatures(context: Context): FeatureVector
}