package pos.features.extractors

import computation.FeatureVector
import pos.parser.ParseHistory

class WordVecUnigramFeatureExtractor() extends POSFeatureExtractor {
  override def extractFeatures(history: ParseHistory): Seq[FeatureVector] = {
    var seqOut = Seq[FeatureVector]()

    // Hidden states
    for (i <- 0 until history.length) {
      val vec = new FeatureVector()
      vec add ("label:" + history.tags(i).replace(":", "%"))
      seqOut :+= vec
    }

    // Visible states
    for (i <- 0 until history.length) {
      val token = history.sentence(i)
      val vec = new FeatureVector()

      vec add ("form:" + history.sentence(i).form)

      for (j <- 0 until token.wordVector.length) {
        vec.add(s"word2vec-$j", token.wordVector(j))
      }

      seqOut :+= vec
    }

    seqOut
  }
}
