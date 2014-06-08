package main

import components.TreeBuilder
import input.ConllParser
import parser.HistoryParser
import features.extractors.BasicFeatureExtractor
import features.labellers.EdgeOnlyDataLabeller

object Main extends App {
  val trainFile = Constants.DEP_TEST

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

  println(parseHistories)
}
