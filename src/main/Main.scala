package main

import components.{TreeBuilder, Tree}
import input.ConllParser
import parser.{ParseDecision, DecisionGenerator}

object Main extends App {
  val trainFile = Constants.MINI_FILE

  val treeBuilder = new TreeBuilder(new ConllParser())
  val trees = treeBuilder.buildTreesFromFile(trainFile)

  val decisionGenerator = new DecisionGenerator()
  val allParseDecisions = trees.map(decisionGenerator.generateDecisions)
}
