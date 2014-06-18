package dependencies.parser.classifiers

import dependencies.features.DataPoint

case class ScoredDataInstance(vector: DataPoint, score: Double)
