package pos.features.extractors

import computation.FeatureVector
import pos.parser.ParseHistory

class TrigramWordVecFeatureExtractor() extends POSFeatureExtractor {
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

    // Visible states
    for (i <- 0 until history.length) {
      val token = history.sentence(i)
      val vec = new FeatureVector()

      vec add ("form:" + token.form)

      for (j <- 0 until token.wordVector.length)
        vec.add(s"word2vec-$j", token.wordVector(j))

      seqOut :+= vec
    }

    seqOut
  }
}
