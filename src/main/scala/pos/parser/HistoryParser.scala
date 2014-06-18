package pos.parser

import pos.components.Token

class HistoryParser {

  def parseHistory(sentence: Seq[Token]) = {
    var contexts = Seq[Context]()
    var tags = Seq[String]()

    contexts :+= new Context()

    sentence.foreach(token => {
      tags :+= token.POS
      contexts :+= contexts.last.add(token)
    })

    ParseHistory(sentence, contexts, tags)
  }
}
