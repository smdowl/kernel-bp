package parser

import components.Token

case class ParseHistory(sentence: Seq[Token], contexts: Seq[Context], parseDecisions: Seq[ParseDecision])
