package features.extractors

import features.FeatureVector
import parser.Context

class BasicFeatureExtractor extends FeatureExtractor {
  override def extractFeatures(context: Context): FeatureVector = {
    val output = new FeatureVector()

    if (context.stack.size > 0)
      output add ("stack-top:" + context.stack.top.form)

    if (context.buffer.size > 0)
      output add ("buffer-top:" + context.buffer.top.form)

    if (context.buffer.size > 1)
      output add ("buffer-2nd-top:" + context.buffer(1).form)

    output
  }
}
