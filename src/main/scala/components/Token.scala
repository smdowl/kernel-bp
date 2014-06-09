package components

case class Token(id: Int,
                 form: String = "_",
                 lemma: String = "_",
                 coarsePOS: String = "_",
                 POS: String = "_",
                 features: Seq[String] = Seq())

class RootToken() extends Token(0, form="ROOT")

class EmptyToken() extends Token(-1, form="_")