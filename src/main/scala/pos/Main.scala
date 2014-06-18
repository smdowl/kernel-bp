package pos

import pos.components.SentenceBulider
import pos.parser.HistoryParser
import input.ConllParser
import app.Constants

object Main extends App {
  val builder = new SentenceBulider(new ConllParser())
  val historyParser = new HistoryParser()

  val parseHistories = getHistories(Constants.DEP_TEST)

  println(parseHistories)

  private def getHistories(file: String) = {
    val sentences = builder.buildSentenceFromFile(file)
    sentences.map(historyParser.parseHistory)
  }
}
