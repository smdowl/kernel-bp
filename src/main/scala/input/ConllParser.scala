package input

class ConllParser extends Parser {

  def tokensFromBlock(block: Seq[TokenDef]): Sentence = {
    block.map(comps => {
      ParsedToken(comps(0).toInt, comps(1), comps(2), comps(3), comps(4), comps(5).split("|"), comps(6).toInt, comps(7))
    })
  }
}
