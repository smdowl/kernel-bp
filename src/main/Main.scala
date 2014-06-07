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

  val tree = TreeBuilder.buildTestTree()
  println(tree)

  def printTreeSizes() = {
    (trees zip allParseDecisions).foreach {
      case (tree: Tree, decisions: Seq[ParseDecision]) =>
        //      println(tree.size)
        //      println(decisions.length)
        println(s"${tree.size * 2 - 2} vs ${decisions.length}")
        println(s"${tree.numEdges * 2} vs ${decisions.length}")
        println()
    }
  }
}
