package pos

import computation.FeatureVector
import pos.components.SentenceBuilder
import pos.parser.HistoryParser
import input.ConllParser
import app.Constants
import pos.features.extractors.{FeatureArrayBuilder, BasicFeatureExtractor}

object Main extends App {
  val builder = new SentenceBuilder(new ConllParser())
  val historyParser = new HistoryParser()

  val parseHistories = getHistories(Constants.DEP_TEST)
  val extractor = new BasicFeatureExtractor

  var featuresSeq = Seq[FeatureVector]()
  parseHistories.foreach(_.contexts.foreach(context => {
    val features = extractor.extractFeatures(context)
    println(features)
    featuresSeq :+= features
  }))

  val featureArray = FeatureArrayBuilder.buildFeatureArray(featuresSeq.toArray)

  println(featureArray)

  private def getHistories(file: String) = {
    val sentences = builder.buildSentenceFromFile(file)
    sentences.map(historyParser.parseHistory)
  }
}
