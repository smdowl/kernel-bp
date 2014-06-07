package main

import components.{TreeBuilder, Tree}
import input.ConllParser
import parser.{ParseDecision, HistoryGenerator}

object Main extends App {
  val trainFile = Constants.MINI_FILE

  val treeBuilder = new TreeBuilder(new ConllParser())
  val trees = treeBuilder.buildTreesFromFile(trainFile)

  val historyGenerator = new HistoryGenerator()

  val parseHistories = trees.map(historyGenerator.generateHistory)

  (trees zip parseHistories).foreach {
    case (tree: Tree, decisions: Seq[ParseDecision]) =>
//      println(tree.size)
//      println(decisions.length)
      println(s"${tree.size * 2 - 2} vs ${decisions.length}")
      println(s"${tree.numEdges * 2} vs ${decisions.length}")
      println()
  }
}
