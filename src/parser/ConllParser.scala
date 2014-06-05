package parser

class ConllParser(filepath: String) extends DepParser(filepath) {

  def tokensFromBlock(block: Seq[TokenDef]): DepSentence = {
    block.map(comps => {
      ParseToken(comps(0).toInt, comps(1), comps(2), comps(3), comps(4), comps(5).split("|"), comps(6).toInt, comps(7))
    })
  }
}
