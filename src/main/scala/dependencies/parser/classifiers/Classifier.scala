package dependencies.parser.classifiers

import dependencies.parser.{ParseDecision, Context}

trait Classifier {
  def getParseDecision(context: Context): ParseDecision
}
