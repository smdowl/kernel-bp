package pos.output

import app.Constants
import breeze.linalg.DenseVector
import computation.FeatureVector
import input.{ConllParser, Parser}
import pos.components.SentenceBuilder
import pos.features.extractors.{FeatureArrayBuilder, FeatureExtractor}
import pos.parser.{ParseHistory, HistoryParser}

object ParsedFeaturesOutput {
  var builder: SentenceBuilder = _
  var historyParser: HistoryParser = _

  def apply(parser: Parser, extractor: FeatureExtractor, source: String = Constants.DEP_TEST) = {
    builder = new SentenceBuilder(parser)
    historyParser = new HistoryParser()

    val parseHistories = getHistories(source)

    var featuresSeq = Seq[Seq[FeatureVector]]()
    parseHistories.foreach(history => {
      val sentenceFeatures = getSentenceFeatures(history, extractor)
      featuresSeq :+= sentenceFeatures
    })

    FeatureArrayBuilder.buildFeatureArray(featuresSeq)
  }

  private def getHistories(file: String) = {
    val sentences = builder.buildSentenceFromFile(file)
    sentences.map(historyParser.parseHistory)
  }

  private def getSentenceFeatures(history: ParseHistory, extractor: FeatureExtractor) = {
    history.contexts.map(extractor.extractFeatures)
  }
}
