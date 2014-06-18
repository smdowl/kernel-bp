package pos.ngram
import pos.components.Token

trait Tagger {
  def train(sentences: Seq[Seq[Token]])
  def label(setence: Seq[String])
}
