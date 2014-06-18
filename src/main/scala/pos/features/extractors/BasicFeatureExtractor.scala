package pos.features.extractors

import computation.FeatureVector
import pos.parser.Context

class BasicFeatureExtractor extends FeatureExtractor {
  override def extractFeatures(context: Context): FeatureVector = {
    val output = new FeatureVector()

    if (context.history.size > 0)
      output add ("stack-top:" + context.history.last)

    output
  }
}
