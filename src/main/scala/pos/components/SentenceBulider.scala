package pos.components

import input.{Parser, ParsedToken}

class SentenceBulider(parser: Parser) {
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
