package main

import components.DepTree
import input.ConllParser

object Main extends App {
  val formatFile = "/Users/shaundowling/Google Drive/UCL/master project/code/history-gen/data/format.conll"
  val trainFile = "/Users/shaundowling/Google Drive/UCL/master project/code/history-gen/data/mini.conll"

  val parser = new ConllParser(trainFile)

  parser.parseLines.foreach(sentence => {
    val sentenceWords = sentence.map(token => token.form)
    println(sentenceWords.mkString(" "))
  })

  val tree = new DepTree(parser.parseLines(0))
  println(tree)
}
