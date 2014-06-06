package input

import scala.io.Source

case class ParseToken(id: Int,
                      form: String,
                      lemma: String,
                      coarsePOS: String,
                      POS: String,
                      features: Seq[String],
                      head: Int,
                      depRel: String)

abstract class DepParser(filepath: String) {

  type TokenDef = Seq[String]
  type DepSentence = Seq[ParseToken]

  var sentences: Seq[DepSentence] = Seq()

  def parseLines: Seq[DepSentence] = {

    if (sentences.length > 0)
      return sentences

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

  protected def tokensFromBlock(block: Seq[Seq[String]]): DepSentence
}