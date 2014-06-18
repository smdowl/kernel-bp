package pos

import pos.components.SentenceBulider
import pos.parser.HistoryParser
import input.ConllParser
import app.Constants
import pos.features.extractors.BasicFeatureExtractor

object Main extends App {
  val builder = new SentenceBulider(new ConllParser())
  val historyParser = new HistoryParser()

  val parseHistories = getHistories(Constants.MINI_TRAIN_FILE)
  val extractor = new BasicFeatureExtractor
  parseHistories.foreach(_.contexts.foreach(context => {
    val features = extractor.extractFeatures(context)
    println(features)
  }))

  println(parseHistories)

  private def getHistories(file: String) = {
    val sentences = builder.buildSentenceFromFile(file)
    sentences.map(historyParser.parseHistory)
  }
}
