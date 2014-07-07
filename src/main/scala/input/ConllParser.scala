package input

import breeze.linalg.DenseVector

class ConllParser extends Parser {

  def tokensFromBlock(block: Seq[TokenDef]): Sentence = {
    block.map(comps => {
      if (hasWordVector(comps))
        ParsedToken(comps(0).toInt, comps(1), comps(2), comps(3), comps(4), comps(5).split("|"), comps(6).toInt, comps(7), toDenseVector(comps(10)))
      else
        ParsedToken(comps(0).toInt, comps(1), comps(2), comps(3), comps(4), comps(5).split("|"), comps(6).toInt, comps(7))
    })
  }

  def hasWordVector(comps: TokenDef) = comps.size > 10

  def toDenseVector(csv: String) = DenseVector(csv.split(",").map(_.toDouble))

}
