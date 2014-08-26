package pos.features.extractors

import computation.FeatureVector
import pos.parser.{ParseHistory, Context}

class UnigramChainOnlyPOSFeatureExtractor extends POSFeatureExtractor {
  override def extractFeatures(history: ParseHistory): Seq[FeatureVector] = {

    var seqOut = Seq[FeatureVector]()

    for (i <- 0 until history.length) {
      val vec = new FeatureVector()
      vec add ("label:" + history.tags(i).replace(":", "%"))
      seqOut :+= vec
    }

    for (i <- 0 until history.length) {
      val vec = new FeatureVector()
      seqOut :+= vec
    }

    seqOut
  }
}
