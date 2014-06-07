package main

import components.{TreeBuilder, Tree}
import input.ConllParser
import parser.{ParseDecision, HistoryParser}

object Main extends App {
  val trainFile = Constants.DEP_TEST

  val treeBuilder = new TreeBuilder(new ConllParser())
  val trees = treeBuilder.buildTreesFromFile(trainFile)

  val decisionGenerator = new HistoryParser()
  val allParseDecisions = trees.map(decisionGenerator.parseHistory)
}
