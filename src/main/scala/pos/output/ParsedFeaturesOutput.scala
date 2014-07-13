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
  var extractor: FeatureExtractor = _

  def apply(parser: Parser, extractor: FeatureExtractor, length: Int = -1, source: String = Constants.DEP_TEST,
             testSource: String = Constants.MINI_TEST_FILE) = {
    this.extractor = extractor
    builder = new SentenceBuilder(parser)
    historyParser = new HistoryParser()

    val trainFeatures = featureVectorsFromSource(source, length)
    val testFeatures = featureVectorsFromSource(testSource, length)

    FeatureArrayBuilder.buildFeatureArray(trainFeatures, testFeatures)
  }

  private def featureVectorsFromSource(source: String, length: Int) = {
    val parseHistories = getHistories(source)

    var featuresSeq = Seq[Seq[FeatureVector]]()
    parseHistories.foreach(history => {
      if (length > 0 && history.contexts.length == length) {
        val sentenceFeatures = getSentenceFeatures(history, extractor)
        featuresSeq :+= sentenceFeatures
      }
    })

    featuresSeq
  }

  private def getHistories(file: String) = {
    val sentences = builder.buildSentenceFromFile(file)
    sentences.map(historyParser.parseHistory)
  }

  private def getSentenceFeatures(history: ParseHistory, extractor: FeatureExtractor): Seq[FeatureVector] = {
    extractor.extractFeatures(history)
  }
}
