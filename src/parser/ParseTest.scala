package parser

object ParseTest extends App {
  val formatFile = "/Users/shaundowling/Google Drive/UCL/master project/code/history-gen/data/format.conll"
  val trainFile = "/Users/shaundowling/Google Drive/UCL/master project/code/history-gen/data/mini.conll"

  val parser = new ConllParser(trainFile, formatFile)

  parser.headers.foreach(println)
  parser.parseLines.foreach(sentence => println(sentence.printSentence))
}
