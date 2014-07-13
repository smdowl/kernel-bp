package pos.features.extractors

import computation.FeatureVector
import pos.parser.{ParseHistory, Context}

class BasicFeatureExtractor extends FeatureExtractor {
  override def extractFeatures(history: ParseHistory): Seq[FeatureVector] = {

    var seqOut = Seq[FeatureVector]()

    for (i <- 0 to history.length) {
      val vec = new FeatureVector()
      val context = history.contexts(i)

      if (i == 0) {
        vec add "label:NONE"
      }

      if (i > 0) {
        vec add ("label:" + history.tags(i-1))
        vec add ("feature-prevtoken:" + history.sentence(i-1).form)
      }

//      if (i > 1) {
//        vec add ("feature-prevprevtoken:" + history.sentence(i-2).form)
//      }

      if (i < history.length - 1)
        vec add ("feature-nexttoken:" + history.sentence(i+1).form)

      if (i < history.length)
        vec add ("feature-token:" + history.sentence(i).form)

      addContextFeaturesToVector(vec, context)

      seqOut :+= vec
    }

    seqOut
  }

  private def addContextFeaturesToVector(vec: FeatureVector, context: Context) = {

  }
}
