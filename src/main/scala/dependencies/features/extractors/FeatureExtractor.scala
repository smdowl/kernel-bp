package dependencies.features.extractors

import dependencies.parser.Context
import dependencies.features.FeatureVector

trait FeatureExtractor {
  def extractFeatures(context: Context): FeatureVector
}