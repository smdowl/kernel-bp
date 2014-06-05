package parser

import components.{Sentence, Token}
import java.io.File
import scala.io.Source
import scala.collection.immutable.HashMap

class ConllParser(filepath: String, formatFilepath: String) {
  
  val headers: Seq[String] = {
    val lines = Source.fromFile(formatFilepath).getLines()
    lines.flatMap(line => line.split("\t")).toSeq
  }

  def parseLines: Seq[Sentence] = {
    var sentences: Seq[Sentence] = Seq()
    var block: Seq[Seq[String]] = Seq()

    Source.fromFile(filepath).getLines().foreach(line => {
      val comps = line.split("\t")

      if (comps.length <= 1) {
        sentences = sentences :+ tokensFromBlock(block)
        block = Seq()
      } else {
        block = block :+ comps.toSeq
      }
    })

    sentences
  }

  def tokensFromBlock(block: Seq[Seq[String]]): Sentence = {
    val sentence = new Sentence()

    block.foreach(comps => {
      sentence.append(Token(comps(0).toInt, comps(1), comps(2), comps(3), comps(4), comps(5).split("|"), comps(6).toInt, comps(7), comps(8), comps(9)))
    })

    sentence
  }
}
