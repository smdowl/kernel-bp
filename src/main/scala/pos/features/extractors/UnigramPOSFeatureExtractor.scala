package pos.features.extractors

import computation.FeatureVector
import pos.parser.{ParseHistory, Context}

class UnigramPOSFeatureExtractor extends POSFeatureExtractor {
  override def extractFeatures(history: ParseHistory): Seq[FeatureVector] = {

    var seqOut = Seq[FeatureVector]()

    for (i <- 0 until history.length) {
      val vec = new FeatureVector()
      vec add ("label:" + history.tags(i).replace(":", "%"))
      seqOut :+= vec
    }

    for (i <- 0 until history.length) {
      val vec = new FeatureVector()
      vec add ("form:" + history.sentence(i).form)
      seqOut :+= vec
    }

    seqOut
  }
}
