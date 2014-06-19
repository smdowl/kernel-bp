package pos.taggers

import pos.components.{SentenceBuilder, Token}

object NGramTagger extends App {
  val sentence = SentenceBuilder.buildTest()

  val tagger = new NGramTagger(1)
  tagger.train(Seq(sentence))
}

class NGramTagger(n: Int) extends Tagger {
  var count: Int = 0
  var model: Map[String, Double] = Map()
  var uniqueTags: Set[String] = Set()

  def train(sentences: Seq[Seq[Token]]) = {
    sentences.foreach(trainFromSentence)
  }

  private def trainFromSentence(sentence: Seq[Token]) = {
    var currHist = getNegTags

    sentence.foreach(token => {
      uniqueTags += token.POS
      currHist = learn(currHist, token.POS)
    })
  }

  private def getNegTags: Seq[String] = {
    var out = Seq[String]()

    for (i <- 1 to n)
      out :+= s"S-$i"

    out
  }

  private def learn(hist: Seq[String], tag: String): Seq[String] = {
    val str = hist.mkString(",")

    model += str -> (model.getOrElse(str, 0.0) + 1.0)
    count += 1

    hist.drop(1) :+ tag
  }

  def label(setence: Seq[String]) = {

  }
}
