package pos.features.extractors

import computation.FeatureVector
import pos.parser.Context

class BasicFeatureExtractor extends FeatureExtractor {
  override def extractFeatures(context: Context): FeatureVector = {
    val output = new FeatureVector()

    if (context.prev != null)
      output add ("label:" + context.prev.token.POS)

    output add ("feature-token:" + context.token.form)
    if (context.prev != null)
      output add ("feature-prevtoken:" + context.prev.token.form)

    output
  }
}
