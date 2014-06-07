package main

import components.{TreeBuilder, Tree}
import input.ConllParser
import parser.HistoryGenerator

object Main extends App {
  val trainFile = Constants.MINI_FILE

  val parser = new ConllParser(trainFile)

  parser.parseLines.foreach(sentence => {
    val sentenceWords = sentence.map(token => token.form)
    println(sentenceWords.mkString(" "))
  })
  println()

  val trees = parser.parseLines.map(TreeBuilder.buildTree)
  val historyGenerator = new HistoryGenerator()

  val parseHistories = trees.map(historyGenerator.generateHistory)
  parseHistories.foreach(history => {
    history.foreach(println)
    println()
  })
}
