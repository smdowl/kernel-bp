package input

import scala.io.Source

case class ParsedToken(id: Int,
                      form: String,
                      lemma: String,
                      coarsePOS: String,
                      POS: String,
                      features: Seq[String],
                      head: Int,
                      depRel: String)

abstract class Parser {

  type TokenDef = Seq[String]
  type Sentence = Seq[ParsedToken]

  def parseLines(filepath: String): Seq[Sentence] = {
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