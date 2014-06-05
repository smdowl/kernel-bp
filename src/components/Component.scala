package components

abstract class Component {

}

case class Token(id: Int,
                 form: String,
                 lemma: String,
                 coarsePOS: String,
                 POS: String,
                 features: Seq[String],
                 head: Int,
                 depRel: String,
                 projHead: String,
                 projDepRel: String) extends Component

class Sentence(initial: Seq[Token] = Seq()) extends Component {
  var tokens: Seq[Token] = initial

  def append(token: Token) {
    tokens = tokens :+ token
  }

  def printSentence = tokens.map(_.form).mkString(" ")
}
