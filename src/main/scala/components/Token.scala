package components

case class Token(id: Int,
                 form: String = "_",
                 lemma: String = "_",
                 coarsePOS: String = "_",
                 POS: String = "_",
                 features: Seq[String] = Seq()) extends Ordered[Token] {

  override def compare(that: Token): Int = this.id compare that.id
}

class RootToken() extends Token(0, form="ROOT")

class EmptyToken() extends Token(-1, form="_")