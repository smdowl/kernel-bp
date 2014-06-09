package app

import components.TreeBuilder
import input.ConllParser
import parser.HistoryParser
import features.extractors.BasicFeatureExtractor
import features.labellers.EdgeOnlyDataLabeller
import parser.classifiers.KNNClassifier

object Main extends App {
  val trainFile = Constants.MINI_FILE

  val treeBuilder = new TreeBuilder(new ConllParser())
  val trees = treeBuilder.buildTreesFromFile(trainFile)

  val historyParser = new HistoryParser()
  val parseHistories = trees.map(historyParser.parseHistory)

  val history = parseHistories(0)
  val featureExtractor = new BasicFeatureExtractor()
  val dataLabeller = new EdgeOnlyDataLabeller()

  val features = history.contexts.map(context => {
    featureExtractor.extractFeatures(context)
  })

  dataLabeller.initialise(history.parseDecisions)
  val labels = history.parseDecisions.map(decision => {
    dataLabeller.label(decision)
  })

  (history.contexts zip features).foreach(pair => {
    println(s"# -> ${pair._2}")
  })

  val knnClassifier = new KNNClassifier(1, featureExtractor, dataLabeller)
  knnClassifier.train(parseHistories)

  (history.contexts zip history.parseDecisions).foreach(pair => {
    val predicted = knnClassifier.getParseDecision(pair._1)
    val actual = pair._2
//    assert(predicted.equals(actual), "Should return same.")

    println(s"${predicted} => ${actual}")
  })

  println(parseHistories)
}
