package pos.features.extractors

import computation.FeatureVector
import pos.parser.{ParseHistory, Context}

class BigramPOSFeatureExtractor extends POSFeatureExtractor {
  override def extractFeatures(history: ParseHistory): Seq[FeatureVector] = {

    var seqOut = Seq[FeatureVector]()
    var prev: String = null
    for (i <- 0 until history.length) {
      val vec = new FeatureVector()
      val label = history.tags(i).replace(":", "%")
      vec add ("label:" + label)

      if (prev != null)
        vec add ("prev-label:" + prev)

      prev = label
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
