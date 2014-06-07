package parser

case class ParseHistory(contexts: Seq[Context], parseDecisions: Seq[ParseDecision])
