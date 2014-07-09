package pos.parser

import pos.components.Token

class HistoryParser {

  def parseHistory(sentence: Seq[Token]) = {
    var contexts = Seq[Context]()
    var tags = Seq[String]()

    var prev = new Context()
    contexts :+= prev

    sentence.foreach(token => {
      tags :+= token.POS

      val newContext = contexts.last.add(token)
      contexts :+= newContext

      newContext.prev = prev
      prev.next = newContext
      prev = newContext
    })

    ParseHistory(sentence, contexts, tags)
  }
}
