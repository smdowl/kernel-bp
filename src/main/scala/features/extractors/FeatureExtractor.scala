package features.extractors

import parser.Context
import features.FeatureVector

trait FeatureExtractor {
  def extractFeatures(context: Context): FeatureVector
}