package pos.parser

import pos.components.Token

case class ParseHistory(sentence: Seq[Token], contexts: Seq[Context], tags: Seq[String]) {
  def length = sentence.length
}
