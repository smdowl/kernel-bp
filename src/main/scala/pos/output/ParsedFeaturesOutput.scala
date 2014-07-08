package pos.output

import app.Constants
import breeze.linalg.DenseVector
import computation.FeatureVector
import input.{ConllParser, Parser}
import pos.components.SentenceBuilder
import pos.features.extractors.{FeatureArrayBuilder, BasicFeatureExtractor, FeatureExtractor}
import pos.parser.HistoryParser

object ParsedFeaturesOutput {
  var builder: SentenceBuilder = _
  var historyParser: HistoryParser = _

  def apply(parser: Parser, extractor: FeatureExtractor): Array[DenseVector[Double]] = {
    builder = new SentenceBuilder(parser)
    historyParser = new HistoryParser()

    val parseHistories = getHistories(Constants.DEP_TEST)

    var featuresSeq = Seq[FeatureVector]()
    parseHistories.foreach(_.contexts.foreach(context => {
      val features = extractor.extractFeatures(context)
      println(features)
      featuresSeq :+= features
    }))

    FeatureArrayBuilder.buildFeatureArray(featuresSeq.toArray)
  }

  private def getHistories(file: String) = {
    val sentences = builder.buildSentenceFromFile(file)
    sentences.map(historyParser.parseHistory)
  }
}
