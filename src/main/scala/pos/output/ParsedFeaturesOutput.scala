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

  def apply(parser: Parser, extractor: FeatureExtractor, length: Int, source: String = Constants.DEP_TEST,
             testSource: String = Constants.MINI_TEST_FILE) = {
    this.extractor = extractor
    builder = new SentenceBuilder(parser)
    historyParser = new HistoryParser()

    val trainFeatures = featureVectorsFromSource(source)
    val testFeatures = featureVectorsFromSource(testSource)

    FeatureArrayBuilder.buildFeatureArray(trainFeatures, testFeatures, length)
  }

  private def featureVectorsFromSource(source: String) = {
    val parseHistories = getHistories(source)

    var featuresSeq = Seq[Seq[FeatureVector]]()
    parseHistories.foreach(history => {
      val sentenceFeatures = getSentenceFeatures(history, extractor)
      featuresSeq :+= sentenceFeatures
    })

    featuresSeq
  }

  private def getHistories(file: String) = {
    val sentences = builder.buildSentenceFromFile(file)
    sentences.map(historyParser.parseHistory)
  }

  private def getSentenceFeatures(history: ParseHistory, extractor: FeatureExtractor) = {
    history.contexts.map(extractor.extractFeatures)
  }
}
