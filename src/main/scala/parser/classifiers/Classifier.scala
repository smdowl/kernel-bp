package parser.classifiers

import parser.{ParseDecision, Context}

trait Classifier {
  def getParseDecision(context: Context): ParseDecision
}
