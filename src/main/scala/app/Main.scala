package main

import components.TreeBuilder
import input.ConllParser
import parser.HistoryParser

object Main extends App {
  val trainFile = Constants.DEP_TEST

  val treeBuilder = new TreeBuilder(new ConllParser())
  val trees = treeBuilder.buildTreesFromFile(trainFile)

  val historyParser = new HistoryParser()
  val parseHistories = trees.map(historyParser.parseHistory)

  println(parseHistories)
}
