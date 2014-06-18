package pos.components

import input.{ConllParser, Parser, ParsedToken}
import app.Constants

object SentenceBuilder extends App {
  def buildTest(): Seq[Token] = {
    val builder = new SentenceBuilder(new ConllParser)
    builder.buildSentenceFromFile(Constants.DEP_TEST)(0)
  }
}

class SentenceBuilder(parser: Parser) {
  def buildSentenceFromFile(filepath: String): Seq[Seq[Token]] = {
    val sentences = parser.parse(filepath)
    sentences.map(buildSentence)
  }

  private def buildSentence(parsedSentence: Iterable[ParsedToken]): Seq[Token] = {
    parsedSentence.map(token =>{
      new Token(token.id, token.form, token.POS)
    }).toSeq
  }
}
