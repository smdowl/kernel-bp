package parser.classifiers

import features.DataPoint

case class ScoredDataInstance(vector: DataPoint, score: Double)
