package pos.parser

import pos.components.Token

class Context(val history: Seq[Token] = Seq[Token]()) {
  def add(token: Token) = new Context(history :+ token)
}
