package input

import scala.io.Source



abstract class Parser {

  type TokenDef = Seq[String]
  type Sentence = Seq[ParsedToken]

  def parse(filepath: String): Seq[Sentence] = {
    var sentences: Seq[Sentence] = Seq()
    var block: Seq[TokenDef] = Seq()

    Source.fromFile(filepath).getLines().foreach(line => {
      val comps = line.split("\t")

      if (comps.length <= 1) {
        sentences = sentences :+ tokensFromBlock(block)
        block = Seq()
      } else {
        block = block :+ comps.toSeq
      }
    })

    sentences :+ tokensFromBlock(block)
  }

  protected def tokensFromBlock(block: Seq[Seq[String]]): Sentence
}