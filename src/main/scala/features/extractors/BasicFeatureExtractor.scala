package features.extractors

import features.FeatureVector

class BasicFeatureExtractor extends FeatureExtractor {
  override def extractFeatures(context: Context): FeatureVector = {
    var features = Map[String, Double]()
    features += ("stack-top:" + context.stack.top.form -> 1.0)
    features += ("buffer-top:" + context.buffer.top.form -> 1.0)
    features += ("buffer-2nd-top:" + context.buffer(1).form -> 1.0)

    new FeatureVector(features.toMap)
  }
}
