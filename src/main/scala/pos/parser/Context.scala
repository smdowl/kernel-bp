package pos.parser

import pos.components.Token

class Context(val history: Seq[Token] = Seq[Token](), var prev: Context = null, var next: Context = null) {
  def add(token: Token) = new Context(history :+ token)
  def token = if (history.length > 0) history.last else null
}
