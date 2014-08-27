package pos.features.extractors

import computation.FeatureVector
import pos.parser.{ParseHistory, Context}

class TrigramChainOnlyPOSFeatureExtractor extends POSFeatureExtractor {
  override def extractFeatures(history: ParseHistory): Seq[FeatureVector] = {

    var seqOut = Seq[FeatureVector]()
    // Hidden states
    var prev: String = null
    var prevPrev: String = null
    for (i <- 0 until history.length) {
      val vec = new FeatureVector()
      val label = history.tags(i).replace(":", "%")
      vec add ("label:" + label)

      if (prev != null) {
        vec add ("prev-label:" + prev)
        prevPrev = prev
      }

      if (prevPrev != null) {
        vec add ("prev-prev-label:" + prevPrev)
      }

      prev = label
      seqOut :+= vec
    }

    for (i <- 0 until history.length) {
      val vec = new FeatureVector()
      vec add "None"
      seqOut :+= vec
    }

    seqOut
  }
}
