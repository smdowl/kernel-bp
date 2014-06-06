package main

import components.{DepTreeBuilder, DepTree}
import input.ConllParser
import parser.HistoryGenerator

object Main extends App {
  final val MINI_FILE = "/Users/shaundowling/Google Drive/UCL/master project/code/history-gen/data/mini.conll"
  final val DEP_TEST = "/Users/shaundowling/Google Drive/UCL/master project/code/history-gen/data/test/test.conll"

  val trainFile = DEP_TEST


  val parser = new ConllParser(trainFile)

  parser.parseLines.foreach(sentence => {
    val sentenceWords = sentence.map(token => token.form)
    println(sentenceWords.mkString(" "))
  })
  println()

  val trees = parser.parseLines.map(DepTreeBuilder.buildTree)
  val historyGenerator = new HistoryGenerator()

  val parseHistories = trees.map(historyGenerator.generateHistory)
  parseHistories(0).foreach(println)
}
